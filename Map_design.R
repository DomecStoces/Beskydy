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

# Parse the complex GPS strings into Decimal Degrees
df_parsed <- df %>%
  mutate(
    # Use regular expressions to extract all numbers (including decimals) from the string
    # This smartly ignores the ° and ´´ symbols which can be tricky to parse natively
    nums = str_extract_all(GPS, "\\d+\\.?\\d*"),
    
    # Extract Latitude components (first 3 numbers)
    lat_deg = as.numeric(sapply(nums, `[`, 1)),
    lat_min = as.numeric(sapply(nums, `[`, 2)),
    lat_sec = as.numeric(sapply(nums, `[`, 3)),
    
    # Extract Longitude components (next 3 numbers)
    lon_deg = as.numeric(sapply(nums, `[`, 4)),
    lon_min = as.numeric(sapply(nums, `[`, 5)),
    lon_sec = as.numeric(sapply(nums, `[`, 6)),
    
    # Convert DMS to Decimal Degrees formula: Degrees + (Minutes/60) + (Seconds/3600)
    Lat = lat_deg + (lat_min / 60) + (lat_sec / 3600),
    Lon = lon_deg + (lon_min / 60) + (lon_sec / 3600)
  ) %>%
  # Clean up the temporary parsing columns
  select(-nums, -lat_deg, -lat_min, -lat_sec, -lon_deg, -lon_min, -lon_sec)

# ---------------------------------------------------------
# 2. CONVERT TO SPATIAL OBJECTS
# ---------------------------------------------------------

# Convert the dataframe to an sf spatial object using EPSG:4326 (WGS84)
sites_sf <- st_as_sf(df_parsed, coords = c("Lon", "Lat"), crs = 4326)

# Convert to a terra SpatVector (which tidyterra prefers)
sites_vect <- vect(sites_sf)

# ---------------------------------------------------------
# 3. FETCH ELEVATION DATA (DEM)
# ---------------------------------------------------------

# Download 30-second resolution elevation data for Czechia (downloads to temp dir)
cz_dem <- elevation_30s(country = "CZE", path = tempdir())

# To make the map look nice and render fast, let's crop the elevation raster 
# to just the Beskids region based on the extent of your points.
# We add a small buffer (0.05 degrees, roughly 5km) around the bounding box.
bbox <- ext(sites_vect)
bbox_buffered <- ext(bbox[1]-0.05, bbox[2]+0.05, bbox[3]-0.05, bbox[4]+0.05)

# Crop the digital elevation model
beskids_dem <- crop(cz_dem, bbox_buffered)

# ---------------------------------------------------------
# 4. CREATE THE MAP WITH TIDYTERRA
# ---------------------------------------------------------

# Generate the plot
map_plot <- ggplot() +
  # 1. Add the elevation raster
  geom_spatraster(data = beskids_dem) +
  # Use tidyterra's built-in whitebox palettes which are excellent for elevation
  scale_fill_whitebox_c(
    palette = "muted",
    name = "Elevation\n(m a.s.l.)",
    na.value = "transparent"
  ) +
  
  # 2. Add your 38 site locations over the raster
  geom_spatvector(
    data = sites_vect, 
    aes(color = Upper.canopy), 
    size = 3.5, 
    alpha = 0.9
  ) +
  
  # Set colors manually for the canopy types present in your dataset
  scale_color_manual(
    values = c("Spruce" = "#117733", "Beech" = "#D55E00"),
    name = "Upper Canopy"
  ) +
  
  # 3. Formatting
  labs(
    title = "Elevational Gradient of Study Sites",
    subtitle = "Moravian-Silesian Beskids Mts.",
    x = "Longitude",
    y = "Latitude"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(face = "bold", size = 14),
    legend.position = "right",
    panel.grid = element_blank()
  )

# Display the map
print(map_plot)

