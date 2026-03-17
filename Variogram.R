coords <- data.frame(
  Locality = 1:20,
  Latitude = c(
    50 + 32/60 + 30.05/3600,   # 1
    50 + 30/60 + 42.72/3600,   # 2
    50 + 30/60 + 22.64/3600,   # 3
    50 + 31/60 + 36.44/3600,   # 4
    50 + 31/60 + 40.13/3600,   # 5
    50 + 32/60 + 44.70/3600,   # 6
    50 + 33/60 + 33.68/3600,   # 7
    50 + 34/60 + 21.58/3600,   # 8
    50 + 35/60 + 16.41/3600,   # 9
    50 + 35/60 + 24.64/3600,   # 10
    50 + 35/60 + 57.02/3600,   # 11
    50 + 37/60 + 28.17/3600,   # 12
    50 + 37/60 + 53.25/3600,   # 13
    50 + 38/60 + 13.17/3600,   # 14
    50 + 38/60 + 8.31/3600,    # 15
    50 + 40/60 + 26.54/3600,   # 16
    50 + 41/60 + 39.80/3600,   # 17
    50 + 41/60 + 32.08/3600,   # 18
    50 + 46/60 + 39/3600,      # 19
    50 + 46/60 + 31/3600       # 20
  ),
  Longitude = c(
    13 + 26/60 + 28.39/3600,   # 1
    13 + 25/60 + 27.43/3600,   # 2
    13 + 24/60 + 30.10/3600,   # 3
    13 + 19/60 + 57.64/3600,   # 4
    13 + 20/60 + 24.96/3600,   # 5
    13 + 17/60 + 3.77/3600,    # 6
    13 + 17/60 + 19.52/3600,   # 7
    13 + 21/60 + 38.15/3600,   # 8
    13 + 19/60 + 36.85/3600,   # 9
    13 + 21/60 + 31.69/3600,   # 10
    13 + 22/60 + 32.76/3600,   # 11
    13 + 23/60 + 48.48/3600,   # 12
    13 + 24/60 + 2.11/3600,    # 13
    13 + 39/60 + 55.43/3600,   # 14
    13 + 40/60 + 5.60/3600,    # 15
    13 + 32/60 + 30.66/3600,   # 16
    13 + 34/60 + 34.51/3600,   # 17
    13 + 38/60 + 6.10/3600,    # 18
    14 + 5/60 + 27.01/3600,    # 19
    14 + 6/60 + 11.05/3600     # 20
  )
)
library(sf)
coords_sf  <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = 4326)
coords_utm <- st_transform(coords_sf, crs = 32633)  # UTM 33N
coords_clean <- coords_utm %>%
  mutate(
    X = st_coordinates(.)[, 1],
    Y = st_coordinates(.)[, 2],
    Locality = as.character(Locality)
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(Locality, X, Y)
cwm_clean <- cwm_clean %>% left_join(coords_clean, by = "Locality")
df <- cwm_clean %>%
  transmute(
    Year      = factor(Year),
    Locality  = factor(Locality),
    Month     = factor(Month),
    Exposition2 = as.numeric(scale(as.numeric(Exposition2))),
    
    Altitude_scaled,
    
    X_km = (X - mean(X, na.rm = TRUE)) / 1000,
    Y_km = (Y - mean(Y, na.rm = TRUE)) / 1000,
    
    Moisture_cwm, Wings_cwm, Distribution_cwm, Body_size_cwm,
    Bioindication_cwm, Dietary_cwm
  ) %>%
  tidyr::drop_na()

# Calculate adaptive basis dimension (k)
k_xy <- max(6, min(10, nrow(dplyr::distinct(df, X_km, Y_km)) - 1))

df <- read_excel("df.xlsx", sheet = "Sheet1")

# Correlogram (autocorrelation using Moran’s I based on site-averaged Pearson residuals)
library(gstat)
library(sp)
library(spdep)

df$resid <- residuals(mod_gam1, type = "pearson")
df_site_res <- df %>%
  group_by(Locality, X_km, Y_km) %>%
  summarise(mean_res = mean(resid, na.rm = TRUE), .groups = "drop")
coords <- as.matrix(df_site_res[,c("X_km","Y_km")])
nb <- dnearneigh(coords, 0, 10)   
lw <- nb2listw(nb, style = "W")
moran.test(df_site_res$mean_res, lw)
coordinates(df_site_res) <- ~X_km + Y_km
vg <- variogram(mean_res ~ 1,
                data = df_site_res,
                cutoff = 40,
                width = 2,
                cressie = TRUE)
plot(vg, main = "Empirical variogram of GAM residuals")