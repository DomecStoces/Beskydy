library(tidyverse)
library(sf)
library(terra)
library(geodata)
library(rayshader)
library(readxl)
library(ggspatial)
library(ggnewscale)

# ---------------------------------------------------------
# 1. READ AND PREPARE THE DATA
# ---------------------------------------------------------

# Read your dataset
df <- read_excel("map.xlsx", sheet = "List1")

df_parsed <- df %>%
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
  )

sites_sf <- st_as_sf(df_parsed, coords = c("Lon", "Lat"), crs = 4326)

# ---------------------------------------------------------
# 2. FETCH ELEVATION & GENERATE HILLSHADE
# ---------------------------------------------------------

cz_dem <- elevation_30s(country = "CZE", path = tempdir())

# Crop DEM
bbox <- ext(vect(sites_sf))
bbox_buffered <- ext(bbox[1]-0.05, bbox[2]+0.05, bbox[3]-0.05, bbox[4]+0.05)
beskids_dem <- crop(cz_dem, bbox_buffered)

# Calculate slope and aspect to generate the terrain shadows (Hillshade)
slope <- terrain(beskids_dem, "slope", unit = "radians")
aspect <- terrain(beskids_dem, "aspect", unit = "radians")
hillshade <- shade(slope, aspect, angle = 45, direction = 315)

# ---------------------------------------------------------
# 3. BUILD THE PUBLICATION 2D MAP
# ---------------------------------------------------------

pub_map <- ggplot() +
  
  # 1. Base Layer: The Hillshade (shadows)
  geom_spatraster(data = hillshade, show.legend = FALSE) +
  scale_fill_gradient(low = "black", high = "white", na.value = "transparent") +
  
  # 2. Second Layer: The Elevation Colors
  # We use alpha = 0.6 to make it slightly transparent so shadows show through
  new_scale_fill() + # Tells ggplot we are using a second fill scale
  geom_spatraster(data = beskids_dem, alpha = 0.6) +
  scale_fill_whitebox_c(
    palette = "muted", 
    name = "Elevation\n(m a.s.l.)",
    na.value = "transparent"
  ) +
  
  # 3. Third Layer: Your Site Points
  # Using shape 21 gives the points a distinct black border so they pop
  geom_spatvector(
    data = vect(sites_sf), 
    aes(color = Upper.canopy), 
    size = 3.5
  ) +
  scale_color_manual(
    values = c("Spruce" = "#117733", "Beech" = "#D55E00"), 
    name = "Upper Canopy"
  ) +
  
  # 4. Manuscript Requirements: North Arrow & Scale Bar
  annotation_scale(location = "br", width_hint = 0.2, text_cex = 0.8) +
  annotation_north_arrow(
    location = "tr", 
    which_north = "true", 
    style = north_arrow_fancy_orienteering,
    height = unit(1, "cm"),
    width = unit(1, "cm")
  ) +
  
  # 5. Clean, Professional Formatting
  theme_minimal() +
  theme(
    panel.grid = element_blank(),
    panel.border = element_rect(fill = NA, color = "black", linewidth = 1), # Sharp border
    axis.title = element_blank(),
    legend.position = "right",
    legend.background = element_rect(fill = "white", color = NA)
  )

# Display the map
print(pub_map)

# Save a high-resolution version for your manuscript draft (300 DPI)
# ggsave("beskids_manuscript_map.png", plot = pub_map, width = 8, height = 6, dpi = 300)
