library(tidyverse)
library(sf)
library(terra)
library(geodata)
library(rayshader)
library(readxl)
library(ggspatial)
library(ggnewscale)
library(magick)

# ---------------------------------------------------------
# 1. READ AND PREPARE THE DATA
# ---------------------------------------------------------

# Read your dataset
df <- read_excel("map.xlsx", sheet = "List1")

# Parse GPS into Decimal Degrees
df_parsed <- df %>%
  # 1. Remove any completely blank rows imported from Excel
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
  # 2. Remove any rows that failed to parse properly (prevents the !anyNA error)
  filter(!is.na(Lat) & !is.na(Lon))

# Convert to an sf spatial object
sites_sf <- st_as_sf(df_parsed, coords = c("Lon", "Lat"), crs = 4326)

# ---------------------------------------------------------
# 2. FETCH, PROJECT DEM, & CREATE MATRIX
# ---------------------------------------------------------

cz_dem <- elevation_30s(country = "CZE", path = tempdir())

# Increased buffer to 0.1 to guarantee NO points are outside the extent
bbox <- ext(vect(sites_sf))
bbox_buffered <- ext(bbox[1]-0.1, bbox[2]+0.1, bbox[3]-0.1, bbox[4]+0.1)
beskids_dem <- crop(cz_dem, bbox_buffered)

# PROJECT to UTM Zone 33N (meters)
beskids_dem_proj <- project(beskids_dem, "EPSG:32633")
sites_sf_proj <- st_transform(sites_sf, 32633)

el_matrix <- raster_to_matrix(beskids_dem_proj)

# ---------------------------------------------------------
# 3. GENERATE POINT OVERLAYS (WITH YOUR FIXES)
# ---------------------------------------------------------

# Explicit numeric extent vector (Fixes alignment/invisible points)
dem_extent <- c(
  xmin(beskids_dem_proj),
  xmax(beskids_dem_proj),
  ymin(beskids_dem_proj),
  ymax(beskids_dem_proj)
)

spruce_sf <- sites_sf_proj %>% filter(Upper.canopy == "Spruce")
beech_sf <- sites_sf_proj %>% filter(Upper.canopy == "Beech")

# Point size reduced to 5 (Fixes the "giant blob" issue)
spruce_overlay <- generate_point_overlay(
  spruce_sf,
  extent = dem_extent,
  heightmap = el_matrix,
  color = "#117733",
  size = 5,
  pch = 16
)

beech_overlay <- generate_point_overlay(
  beech_sf,
  extent = dem_extent,
  heightmap = el_matrix,
  color = "#D55E00",
  size = 5,
  pch = 16
)

# ---------------------------------------------------------
# 4. RENDER THE 2D MAP
# ---------------------------------------------------------

amb_shadow <- ambient_shade(el_matrix, multicore = TRUE, maxsearch = 30)
ray_shadow <- ray_shade(el_matrix, multicore = TRUE, zscale = 30)

map_array <- el_matrix %>%
  sphere_shade(texture = "desert") %>%
  add_water(detect_water(el_matrix), color = "desert") %>%
  add_shadow(ray_shadow, 0.5) %>%
  add_shadow(amb_shadow, 0.3) %>%  # Set to 0.3 so ambient shadow is visible
  add_overlay(spruce_overlay) %>%
  add_overlay(beech_overlay)

# Plot to RStudio viewer!
plot_map(map_array)

# save_png(map_array, filename = "beskids_rayshader_final.png")
