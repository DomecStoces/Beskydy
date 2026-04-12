library(tidyverse)
library(sf)
library(terra)
library(tidyterra)
library(geodata)
library(readxl)
library(ggplot2)
library(ggspatial)
library(ggnewscale)
library(magick)

# ---------------------------------------------------------
# 1. READ AND PREPARE THE DATA
# ---------------------------------------------------------

# Read your dataset
df <- read_excel("map.xlsx", sheet = "List1")

# Parse GPS, filtering out any empty or broken rows to prevent errors
df_parsed <- df %>%
  filter(!is.na(GPS) & GPS != "") %>%
  mutate(
    nums = str_extract_all(GPS, "\\d+\\.?\\d*"),
    lat_deg = as.numeric(sapply(nums, `[`, 1)),
    lat_min = as.numeric(sapply(nums, `[`, 2)),
    lat_sec = as.numeric(sapply(nums, `[`, 3)),
    lon_deg = as.numeric(sapply(nums, `[`, 4)),
    lon_min = as.numeric(sapply(nums, `[`, 5)),
    lon_sec = as.numeric(sapply(nums, `[`, 6)),
    Lat = lat_deg + (lat_min / 60) + (lat_sec / 3600),
    Lon = lon_deg + (lon_min / 60) + (lon_sec / 3600)
  ) %>%
  filter(!is.na(Lat) & !is.na(Lon))

sites_sf <- st_as_sf(df_parsed, coords = c("Lon", "Lat"), crs = 4326)

# ---------------------------------------------------------
# 2. FETCH, SMOOTH, AND CALCULATE HILLSHADE
# ---------------------------------------------------------

cz_dem <- elevation_30s(country = "CZE", path = tempdir())

# 1. Create an EVEN WIDER buffer (0.15 degrees) for the background math
bbox <- ext(vect(sites_sf))
bbox_wide <- ext(bbox[1]-0.15, bbox[2]+0.15, bbox[3]-0.15, bbox[4]+0.15)
beskids_dem_wide <- crop(cz_dem, bbox_wide)

# 2. Increase resolution on the wide raster
beskids_dem_highres <- disagg(beskids_dem_wide, fact = 5, method = "bilinear")

# 3. Apply Focal smoothing for organic contours
smooth_window <- matrix(1/81, nrow = 9, ncol = 9)
beskids_dem_contours <- focal(beskids_dem_highres, w = smooth_window, na.rm = TRUE)

# 4. Calculate Hillshade
slope <- terrain(beskids_dem_highres, "slope", unit = "radians")
aspect <- terrain(beskids_dem_highres, "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = 45, direction = 315) 

# 5. THE EXPANDED EDGE FIX: Crop to a ~3km buffer (0.03 degrees)
bbox_clean <- ext(bbox[1]-0.03, bbox[2]+0.03, bbox[3]-0.03, bbox[4]+0.03)

hillshade_clean <- crop(hillshade, bbox_clean)
beskids_color_clean <- crop(beskids_dem_highres, bbox_clean)
beskids_contours_clean <- crop(beskids_dem_contours, bbox_clean)

# ---------------------------------------------------------
# 3. BUILD THE FINAL MANUSCRIPT MAP
# ---------------------------------------------------------

final_map <- ggplot() +
  
  # 1. Base Layer: The Grey Hillshade
  geom_spatraster(data = hillshade_clean, show.legend = FALSE, maxcell = Inf) +
  scale_fill_gradientn(
    colours = grey(0:100 / 100),
    na.value = "transparent"
  ) +
  
  # Initialize a new color scale
  new_scale_fill() + 
  
  # 2. Second Layer: Semi-transparent Elevation Colors
  geom_spatraster(data = beskids_color_clean, alpha = 0.6, maxcell = Inf) +
  scale_fill_whitebox_c(
    palette = "muted", 
    name = "Elevation\n[m a.s.l.]",
    na.value = "transparent",
    guide = guide_colorbar(order = 2)
  ) +
  
  # 3. Contour Layer
  geom_spatraster_contour(
    data = beskids_contours_clean,
    maxcell = Inf,
    color = "black",
    linewidth = 0.2, 
    alpha = 0.4,
    binwidth = 50    
  ) +
  
  # 4. Points Layer
  geom_spatvector(
    data = sites_sf, 
    aes(color = Upper.canopy), 
    size = 3.5
  ) +
  scale_color_manual(
    values = c("Spruce" = "#117733", "Beech" = "#D55E00"), 
    name = "Dominant tree species",
    guide = guide_legend(order = 1)
  ) +
  
  # 5. Map Annotations & Formatting
  coord_sf(expand = FALSE) +
  annotation_scale(
    location = "br", 
    width_hint = 0.3, 
    text_cex = 1.0
  ) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true",
    rotation = -10,
    style = north_arrow_fancy_orienteering(),
    height = unit(2, "cm"),
    width = unit(2, "cm")
  ) +
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1),
    axis.title = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA)
  )

# Display the map
print(final_map)
ggsave(
  filename = "beskids_map.pdf", 
  plot = final_map, 
  device = "pdf",
  width = 10,     
  height = 8,   
  units = "in",
  colormodel = "srgb"
)

