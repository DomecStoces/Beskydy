library(sf)
library(dplyr)
library(tidyr)
library(writexl)

# Updated coordinates for 38 localities
coords <- data.frame(
  Locality = 1:38,
  Latitude = c(
    49 + 30/60 + 47.5/3600,   # 1
    49 + 30/60 + 10.7/3600,   # 2
    49 + 29/60 + 2.5/3600,    # 3
    49 + 29/60 + 1.9/3600,    # 4
    49 + 29/60 + 2.0/3600,    # 5
    49 + 29/60 + 4.5/3600,    # 6
    49 + 29/60 + 42.6/3600,   # 7
    49 + 30/60 + 10.9/3600,   # 8
    49 + 30/60 + 15.5/3600,   # 9
    49 + 30/60 + 13.5/3600,   # 10
    49 + 31/60 + 8.6/3600,    # 11
    49 + 30/60 + 57.1/3600,   # 12
    49 + 30/60 + 55.0/3600,   # 13
    49 + 31/60 + 3.9/3600,    # 14
    49 + 31/60 + 19.1/3600,   # 15
    49 + 30/60 + 31.7/3600,   # 16
    49 + 29/60 + 55.2/3600,   # 17
    49 + 28/60 + 57.0/3600,   # 18
    49 + 28/60 + 7.0/3600,    # 19
    49 + 27/60 + 56.5/3600,   # 20
    49 + 28/60 + 44.6/3600,   # 21
    49 + 28/60 + 36.2/3600,   # 22
    49 + 28/60 + 24.6/3600,   # 23
    49 + 28/60 + 28.4/3600,   # 24
    49 + 29/60 + 29.3/3600,   # 25
    49 + 29/60 + 27.8/3600,   # 26
    49 + 30/60 + 32.6/3600,   # 27
    49 + 30/60 + 40.6/3600,   # 28
    49 + 31/60 + 38.5/3600,   # 29
    49 + 31/60 + 17.1/3600,   # 30
    49 + 29/60 + 45.2/3600,   # 31
    49 + 30/60 + 18.9/3600,   # 32
    49 + 30/60 + 17.4/3600,   # 33
    49 + 30/60 + 8.5/3600,    # 34
    49 + 31/60 + 9.6/3600,    # 35
    49 + 28/60 + 46.6/3600,   # 36
    49 + 28/60 + 19.5/3600,   # 37
    49 + 31/60 + 13.5/3600    # 38
  ),
  Longitude = c(
    18 + 20/60 + 37.1/3600,   # 1
    18 + 20/60 + 51.5/3600,   # 2
    18 + 21/60 + 8.7/3600,    # 3
    18 + 21/60 + 23.0/3600,   # 4
    18 + 22/60 + 33.3/3600,   # 5
    18 + 22/60 + 16.0/3600,   # 6
    18 + 21/60 + 3.0/3600,    # 7
    18 + 23/60 + 4.4/3600,    # 8
    18 + 23/60 + 2.0/3600,    # 9
    18 + 24/60 + 14.2/3600,   # 10
    18 + 23/60 + 19.9/3600,   # 11
    18 + 22/60 + 54.4/3600,   # 12
    18 + 22/60 + 22.1/3600,   # 13
    18 + 21/60 + 55.9/3600,   # 14
    18 + 22/60 + 9.4/3600,    # 15
    18 + 19/60 + 24.3/3600,   # 16
    18 + 20/60 + 26.1/3600,   # 17
    18 + 20/60 + 38.2/3600,   # 18
    18 + 21/60 + 19.6/3600,   # 19
    18 + 21/60 + 4.6/3600,    # 20
    18 + 22/60 + 43.3/3600,   # 21
    18 + 22/60 + 54.0/3600,   # 22
    18 + 24/60 + 59.5/3600,   # 23
    18 + 25/60 + 1.5/3600,    # 24
    18 + 21/60 + 0.6/3600,    # 25
    18 + 20/60 + 58.1/3600,   # 26
    18 + 18/60 + 13.2/3600,   # 27
    18 + 18/60 + 10.7/3600,   # 28
    18 + 23/60 + 12.9/3600,   # 29
    18 + 18/60 + 57.4/3600,   # 30
    18 + 21/60 + 34.2/3600,   # 31
    18 + 22/60 + 14.8/3600,   # 32
    18 + 22/60 + 8.1/3600,    # 33
    18 + 22/60 + 20.6/3600,   # 34
    18 + 19/60 + 13.2/3600,   # 35
    18 + 23/60 + 39.6/3600,   # 36
    18 + 23/60 + 34.9/3600,   # 37
    18 + 18/60 + 6.6/3600     # 38
  )
)

coords_sf  <- st_as_sf(coords, coords = c("Longitude", "Latitude"), crs = 4326)

# Transform to UTM 34N (EPSG: 32634) because the longitude is > 18 E
coords_utm <- st_transform(coords_sf, crs = 32634)  

# Extract X and Y coordinates in meters
coords_clean <- coords_utm %>%
  mutate(
    X = st_coordinates(.)[, 1],
    Y = st_coordinates(.)[, 2],
    Locality = as.character(Locality)
  ) %>%
  st_drop_geometry() %>%
  dplyr::select(Locality, X, Y)

# Join with community weighted means data (ensure cwm_clean is loaded in your environment)
df <- df %>% left_join(coords_clean, by = "Locality")

# Build the final data frame for your models/variograms
df <- df %>%
  transmute(
    Year        = factor(Year),
    Locality    = factor(Locality),
    Month       = factor(Month),
    Site.protection = factor(Site.protection), 
    Exposition2 = as.numeric(scale(as.numeric(Exposition2))),
    
    Altitude_scaled,
    
    X_km = (X - mean(X, na.rm = TRUE)) / 1000,
    Y_km = (Y - mean(Y, na.rm = TRUE)) / 1000,
    
    Size, Trophic, Rao
  ) %>%
  tidyr::drop_na()

# Calculate adaptive basis dimension (k)
k_xy <- max(6, min(10, nrow(dplyr::distinct(df, X_km, Y_km)) - 1))

write_xlsx(df, "df1.xlsx")
# Correlogram (autocorrelation using Moran’s I based on site-averaged Pearson residuals)
library(gstat)
library(sp)
library(spdep)

# Find the distance to the nearest neighbor for every point
k1 <- knn2nb(knearneigh(coords, k = 1))
dists <- unlist(nbdists(k1, coords))
max_1nn_dist <- max(dists)
print(max_1nn_dist)

# Calculate the robust Cressie variogram up to 10 km
vgm_cressie <- variogram(
  mean_res ~ 1, 
  locations = ~ X_km + Y_km, 
  data = df_site_res,
  cutoff = 10,       
  width = 1,         
  cressie = TRUE     
)

# Plot it
plot(vgm_cressie, 
     main = "Semi-variogram of residuals (Cressie)", 
     xlab = "Distance (km)", 
     ylab = "Semi-variance",
     pch = 19, cex = 1.2)
