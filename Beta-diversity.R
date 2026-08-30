library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)
library(permute)

# 1. Load data
df <- read_excel("CANOCO_FINAL.xlsx", sheet = "env")
compo_names <- read_excel("CANOCO_FINAL.xlsx", sheet = "sp")

# 2. Initial
metadata <- df
metadata$ID <- 1:nrow(metadata)
metadata_matched <- metadata[metadata$ID %in% compo_names$ID, ]

# 3. Prepare raw community matrix
comm_matrix <- as.matrix(compo_names[, -1])
rownames(comm_matrix) <- compo_names$ID

# 4. AGGREGATE DATA
# Combine metadata and species, then sum species counts per locality
df_combined <- bind_cols(metadata_matched, as.data.frame(comm_matrix))

df_clean <- df_combined %>%
  mutate(
    Month_num = as.numeric(sub(".*\\.(\\d+)\\.", "\\1", Date)),
    Month = factor(Month_num, 
                   levels = c(6, 7, 9, 10), 
                   labels = c("June", "July", "September", "October"))
  ) %>%
  drop_na(Locality, Trees, Altitude, Exposition, Year, Month)

# ======================================================================
# PART A: SPATIAL MODEL (Site-Level Aggregation, N = 38)
# ======================================================================

# Aggregate data across all times to site level
df_spatial <- df_clean %>%
  group_by(Locality) %>%
  summarise(
    Trees = first(Trees),
    Altitude = first(Altitude),
    Exposition = first(Exposition),
    across(all_of(colnames(comm_matrix)), ~sum(., na.rm = TRUE)), 
    .groups = "drop"
  ) %>%
  mutate(
    Locality = as.factor(Locality),
    Trees = as.factor(Trees),
    Altitude_scaled = as.numeric(scale(Altitude)),
    Exposition_midpoint = sapply(strsplit(as.character(Exposition), "_"), function(x) {
      mean(as.numeric(trimws(x)), na.rm = TRUE)
    }),
    Exposition2 = as.numeric(scale(Exposition_midpoint))
  ) %>%
  filter(!is.na(Exposition2) & !is.nan(Exposition2))

# Prepare spatial community matrix
comm_spatial <- as.matrix(df_spatial %>% select(all_of(colnames(comm_matrix))))
rownames(comm_spatial) <- df_spatial$Locality

# Convert to presence/absence and filter empty rows
comm_pa_spatial <- ifelse(comm_spatial > 0, 1, 0)
valid_rows_sp <- rowSums(comm_pa_spatial) > 0
comm_pa_spatial <- comm_pa_spatial[valid_rows_sp, , drop = FALSE]

# Convert to dataframe and safely apply rownames
df_spatial <- as.data.frame(df_spatial[valid_rows_sp, ])
rownames(df_spatial) <- as.character(df_spatial$Locality)

# Calculate Spatial Distance Matrices
dist_jaccard_sp <- sqrt(designdist(comm_pa_spatial, method = "1 - (J / (A + B - J))", terms = "binary"))
dist_simpson_sp <- sqrt(designdist(comm_pa_spatial, method = "1 - (J / pmin(A, B))", terms = "binary"))
dist_richness_sp <- sqrt(designdist(comm_pa_spatial, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary"))

# PERMANOVA 1: Spatial Predictors (Unrestricted permutations, N = 38)
perm_spatial_jaccard <- adonis2(dist_jaccard_sp ~ Trees + Altitude_scaled + Exposition2, 
                                data = df_spatial, permutations = 999, by = "margin")
perm_spatial_simpson <- adonis2(dist_simpson_sp ~ Trees + Altitude_scaled + Exposition2, 
                                data = df_spatial, permutations = 999, by = "margin")
perm_spatial_richness <- adonis2(dist_richness_sp ~ Trees + Altitude_scaled + Exposition2, 
                                 data = df_spatial, permutations = 999, by = "margin")

# 7. PERMDISP: testing variance on spatial (N=38) data
disp_jaccard_sp <- betadisper(dist_jaccard_sp, df_spatial$Trees)
disp_simpson_sp <- betadisper(dist_simpson_sp, df_spatial$Trees)
print("--- PERMDISP Results (Spatial N=38) ---")
print(permutest(disp_jaccard_sp, permutations = 999))
print(permutest(disp_simpson_sp, permutations = 999))

# ======================================================================
# PART B: TEMPORAL MODEL (Repeated Measures, N = 471)
# ======================================================================

# Aggregate data to the sampling event level (Year + Month)
df_temporal <- df_clean %>%
  group_by(Locality, Trees, Altitude, Exposition, Year, Month) %>%
  summarise(across(all_of(colnames(comm_matrix)), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    Locality = as.factor(Locality), 
    Year = as.factor(Year),         
    Trees = as.factor(Trees)
  )

# Prepare temporal community matrix
comm_temporal <- as.matrix(df_temporal %>% select(all_of(colnames(comm_matrix))))
safe_rownames_temp <- make.unique(paste(df_temporal$Locality, df_temporal$Trees, df_temporal$Year, df_temporal$Month, sep = "_"))
rownames(comm_temporal) <- safe_rownames_temp

# Convert to presence/absence and filter empty rows
comm_pa_temp <- ifelse(comm_temporal > 0, 1, 0)
valid_rows_temp <- rowSums(comm_pa_temp) > 0
comm_pa_temp <- comm_pa_temp[valid_rows_temp, , drop = FALSE]
df_temporal <- as.data.frame(df_temporal[valid_rows_temp, ])
rownames(df_temporal) <- safe_rownames_temp[valid_rows_temp]

# Calculate Temporal Distance Matrices
dist_jaccard_temp <- sqrt(designdist(comm_pa_temp, method = "1 - (J / (A + B - J))", terms = "binary"))
dist_simpson_temp <- sqrt(designdist(comm_pa_temp, method = "1 - (J / pmin(A, B))", terms = "binary"))
dist_richness_temp <- sqrt(designdist(comm_pa_temp, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary"))

# Define Restricted Permutations (Blocking by Locality to account for repeated measures)
ctrl <- how(blocks = df_temporal$Locality, nperm = 999)

# PERMANOVA 2: Temporal Predictors (Restricted permutations)
perm_temporal_jaccard <- adonis2(dist_jaccard_temp ~ Year + Month, 
                                 data = df_temporal, permutations = ctrl, by = "margin")
perm_temporal_simpson <- adonis2(dist_simpson_temp ~ Year + Month, 
                                 data = df_temporal, permutations = ctrl, by = "margin")
perm_temporal_richness <- adonis2(dist_richness_temp ~ Year + Month, 
                                  data = df_temporal, permutations = ctrl, by = "margin")

# ======================================================================
# PRINT RESULTS FOR TABLE 1
# ======================================================================
print("##################################################")
print("TABLE 1 - JACCARD (Overall compositional dissimilarity)")
print("--- Spatial Model (Df = 37) ---")
print(perm_spatial_jaccard)
print("--- Temporal Model (Blocked by Site) ---")
print(perm_temporal_jaccard)
print("##################################################")

print("##################################################")
print("TABLE 1 - SIMPSON (Species turnover)")
print("--- Spatial Model (Df = 37) ---")
print(perm_spatial_simpson)
print("--- Temporal Model (Blocked by Site) ---")
print(perm_temporal_simpson)
print("##################################################")

print("##################################################")
print("TABLE 1 - RICHNESS UNIFORMITY (Species richness uniformity)")
print("--- Spatial Model (Df = 37) ---")
print(perm_spatial_richness)
print("--- Temporal Model (Blocked by Site) ---")
print(perm_temporal_richness)
print("##################################################")
