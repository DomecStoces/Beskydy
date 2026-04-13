# Map panel B: The detailed study area

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
library(ggrepel)

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
protected_area <- sites_sf %>%
  # Filter for protected sites (handling potential capitalization differences)
  filter(tolower(Site.protection) == "yes") %>%
  # Combine all points into a single geometry
  st_union() %>%
  # Draw the outer boundary (convex hull) around them
  st_convex_hull()
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
    guide = guide_colorbar(order = 3)
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
  new_scale_fill() + geom_sf(
    data = protected_area,
    aes(fill = "Protected regime", color = "Protected regime"), 
    alpha = 0.2,           
    linewidth = 0.8,       
    linetype = "dashed"    
  ) + scale_fill_manual(
    name = "Nature conservation",
    values = c("Protected regime" = "#0072B2"),
    guide = guide_legend(order = 2) 
  ) +
    scale_color_manual(
      name = "Nature conservation",
      values = c("Protected regime" = "#0072B2"),
      guide = guide_legend(order = 2) 
    ) +
    new_scale_color() +
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
  geom_text_repel(
    data = sites_sf,
    aes(label = Locality, geometry = geometry),
    stat = "sf_coordinates",   # Tells ggrepel how to read the sf coordinates
    size = 3.5,
    color = "black",
    fontface = "bold",
    bg.color = "white",        
    bg.r = 0.10,               
    min.segment.length = 0.5,  
    point.padding = NA         
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

# Panel A of Czech Republic with square around the study area
library(tidyr)
library(stringr)
library(sf)
library(dplyr)
library(ggplot2)
library(rnaturalearth)
library(ggrepel)
library(wdpar)
library(ggspatial)

# Read the raw data
dat_text <- "Locality	GPS	Upper.canopy	Site.protection
1	N49°30´47.5´´ E018°20´37.1´´	Spruce	No
2	N49°30´10.7 E018°20´51.5´´	Beech	No
3	N49°29´02.5´´ E018°21´08.7´´	Beech	No
4	N49°29´01.9´´ E018°21´23.0´´	Spruce	No
5	N49°29´02.0´´ E018°22´33.3´´	Beech	No
6	N49°29´04.5´´ E018°22´16.0´´	Beech	Yes
7	N49°29´42.6´´ E018°21´03.0´´	Beech	Yes
8	N49°30´10.9´´ E018°23´04.4´´	Spruce	No
9	N49°30´15.5´´ E018°23´02.0´´	Spruce	No
10	N49°30´13.5´´ E018°24´14.2´´	Spruce	No
11	N49°31´08.6´´ E018°23´19.9´´	Spruce	No
12	N49°30´57.1´´ E018°22´54.4´´	Beech	Yes
13	N49°30´55.0´´ E018°22´22.1´´	Spruce	No
14	N49°31´03.9´´ E018°21´55.9´´	Spruce	No
15	N49°31´19.1´´ E018°22´09.4´´	Spruce	No
16	N49°30´31.7´´ E018´19´24.3´´	Spruce	No
17	N49°29´55.2´´ E018°20´26.1´´	Spruce	No
18	N49°28´57.0´´ E018°20´38.2´´	Spruce	No
19	N49°28´07.0´´ E018°21´19.6´´	Spruce	Yes
20	N49°27´56.5´´ E018°21´04.6´´	Spruce	Yes
21	N49°28´44.6´´ E018°22´43.3´´	Beech	No
22	N49°28´36.2´´ E018°22´54.0´´	Spruce	No
23	N49°28´24.6´´ E018°24´59.5´´	Spruce	No
24	N49°28´28.4´´ E018°25´01.5´´	Spruce	No
25	N49°29´29.3´´ E018°21´00.6´´	Beech	No
26	N49°29´27.8´´ E018°20´58.1´´	Spruce	No
27	N49°30´32.6´´ E018°18´13.2´´	Beech	No
28	N49°30´40.6´´ E018°18´10.7´´	Beech	No
29	N49°31´38.5´´ E018°23´12.9´´	Spruce	No
30	N49°31´17.1´´ E018°18´57.4´´	Spruce	No
31	N49°29´45.2´´ E018°21´34.2´´	Spruce	Yes
32	N49°30´18.9´´ E018°22´14.8´´	Spruce	Yes
33	N49°30´17.4´´ E018°22´08.1´´	Spruce	No
34	N49°30´08.5´´ E018°22´20.6´´	Spruce	Yes
35	N49°31´09.6´´ E018°19´13.2´´	Beech	No
36	N49°28´46.6´´ E018°23´39.6´´	Spruce	No
37	N49°28´19.5´´ E018°23´34.9´´	Spruce	No
38	N49°31´13.5´´ E018°18´06.6´´	Spruce	No"

dat_raw <- read.table(text = dat_text, header = TRUE, sep = "\t", stringsAsFactors = FALSE)

dat_clean <- dat_raw %>%
  mutate(
    coords_list = str_extract_all(GPS, "[0-9.]+"),
    lat = sapply(coords_list, function(x) as.numeric(x[1]) + as.numeric(x[2])/60 + as.numeric(x[3])/3600),
    lon = sapply(coords_list, function(x) as.numeric(x[4]) + as.numeric(x[5])/60 + as.numeric(x[6])/3600)
  ) %>%
  select(-coords_list)

pts <- st_as_sf(dat_clean, coords = c("lon", "lat"), crs = 4326, remove = FALSE)


# --- 2. CREATE WGS84 BOUNDARIES ---
bb_wgs <- st_bbox(pts)

bb_buffered <- st_bbox(c(
  xmin = as.numeric(bb_wgs["xmin"]) - 0.03,
  ymin = as.numeric(bb_wgs["ymin"]) - 0.03,
  xmax = as.numeric(bb_wgs["xmax"]) + 0.03,
  ymax = as.numeric(bb_wgs["ymax"]) + 0.03
), crs = st_crs(4326))

square_wgs <- st_as_sfc(bb_buffered)


# --- 3. DOWNLOAD BASEMAPS AND PROTECTED AREAS ---
eu <- ne_countries(scale = "medium", continent = "Europe", returnclass = "sf")
cz <- eu %>% filter(admin == "Czechia")

Sys.setenv(CHROMOTE_CHROME = "C:/Program Files/Google/Chrome/Application/chrome.exe")
cz_pa <- wdpa_fetch("CZE", wait = TRUE)

beskids_sf <- cz_pa %>% 
  filter(if_any(where(is.character), ~ grepl("Beskydy", .x, ignore.case = TRUE)))


# --- 4. PROJECT ALL SPATIAL DATA TO EPSG:3035 ---
crs_plot <- 3035  
eu_p <- st_transform(eu, crs_plot)
cz_p <- st_transform(cz, crs_plot)
square_p   <- st_transform(square_wgs, crs_plot)
beskids_p  <- st_transform(beskids_sf, crs_plot)


# --- 5. CALCULATE SQUARE AREA (Now that square_p exists!) ---
square_area <- st_area(square_p)
square_area_km2 <- as.numeric(square_area) / 1e6
bbox_proj <- st_bbox(square_p)
width_km <- as.numeric(bbox_proj["xmax"] - bbox_proj["xmin"]) / 1000
height_km <- as.numeric(bbox_proj["ymax"] - bbox_proj["ymin"]) / 1000

cat(sprintf("Square Width: %.1f km\nSquare Height: %.1f km\nTotal Area: %.1f km^2\n", 
            width_km, height_km, square_area_km2))


# --- 6. PREPARE LABELS AND ZOOM BOUNDARIES ---
# Calculate centroids first
centroids <- st_centroid(eu_p)

label_df <- centroids %>%
  mutate(X = st_coordinates(geometry)[,1],
         Y = st_coordinates(geometry)[,2]) %>%
  st_drop_geometry() %>%
  select(admin, iso_a2, X, Y) %>%
  filter(iso_a2 %in% c("CZ", "DE", "PL", "AT", "SK", "HU", "CH", "SI"))

cz_center <- st_centroid(cz_p)
cz_center_coords <- st_coordinates(cz_center)

x_range <- 700000   
y_range <- 550000   
xlim <- c(cz_center_coords[1] - x_range, cz_center_coords[1] + x_range)
ylim <- c(cz_center_coords[2] - y_range, cz_center_coords[2] + y_range)


# --- 7. BUILD THE MAP ---
p_cz_overview <- ggplot() +
  geom_sf(data = eu_p, fill = "white", color = "grey50", linewidth = 0.25) +
  geom_sf(data = cz_p, fill = "grey80", color = "grey50", linewidth = 0.4) +
  geom_sf(data = beskids_p, fill = "#74c476", color = "#238b45", 
          alpha = 0.8, linewidth = 0.4) +
  geom_sf(data = square_p, fill = "white", color="black", 
          alpha = 0.6, linewidth = 0.8) +
  geom_text_repel(
    data = label_df,
    aes(x = X, y = Y, label = iso_a2),
    size = 5.2,
    color = "black",
    max.overlaps = Inf        
  ) +
  coord_sf(xlim = xlim, ylim = ylim, expand = FALSE) +
  annotation_scale(
    location = "bl",        
    width_hint = 0.25,      
    bar_cols = c("black", "white"), 
    text_cex = 1.1,         
    pad_x = unit(0.5, "cm"), 
    pad_y = unit(0.5, "cm")  
  ) +
  theme_void() + 
  theme(
    panel.background = element_rect(fill = "#e0f3f8", color = NA)
  )
p_cz_overview

# Save it
ggsave(
  filename = "map_panelA.pdf", 
  plot = p_cz_overview, 
  device = "pdf",
  width = 10,     
  height = 8,   
  units = "in",
  colormodel = "srgb"
)
