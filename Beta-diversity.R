# Number of species for each row # 
library(readxl)
library(writexl)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "spiders_compo_names")
df$species_richness <- rowSums(df[, -1] > 0)
write_xlsx(df[, c("ID", "species_richness")], "species_richness.xlsx")

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)
library(readxl)

# 1.
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

df_agg <- df_clean %>%
  group_by(Locality, Trees, Altitude, Exposition, Year, Month) %>%
  summarise(across(all_of(colnames(comm_matrix)), ~sum(., na.rm = TRUE)), .groups = "drop") %>%
  mutate(
    Locality = as.factor(Locality), 
    Year = as.factor(Year),         
    Trees = as.factor(Trees),
    Altitude_scaled = as.numeric(scale(Altitude)),
    Exposition_midpoint = sapply(strsplit(as.character(Exposition), "_"), function(x) {
      mean(as.numeric(trimws(x)), na.rm = TRUE)
    }),
    Exposition2 = as.numeric(scale(Exposition_midpoint))
  ) %>%
  filter(!is.na(Exposition2) & !is.nan(Exposition2))

# 5. Prepare aggregated community matrix and convert to presence/absence
comm_agg <- as.matrix(df_agg %>% select(all_of(colnames(comm_matrix))))
safe_rownames <- make.unique(paste(df_agg$Locality, df_agg$Trees, df_agg$Year, df_agg$Month, sep = "_"))
rownames(comm_agg) <- safe_rownames
# Convert to presence/absence
comm_pa_agg <- ifelse(comm_agg > 0, 1, 0)
# Filter out empty rows
valid_rows <- rowSums(comm_pa_agg) > 0
comm_pa_agg <- comm_pa_agg[valid_rows, , drop = FALSE]
# Subset df_agg and apply the exact same filtered row names
df_agg <- as.data.frame(df_agg[valid_rows, ])
rownames(df_agg) <- safe_rownames[valid_rows]

# 6. Calculate independent beta-diversity components
dist_jaccard <- designdist(comm_pa_agg, method = "1 - (J / (A + B - J))", terms = "binary")
dist_simpson <- designdist(comm_pa_agg, method = "1 - (J / pmin(A, B))", terms = "binary")
dist_richness <- designdist(comm_pa_agg, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary")

dist_jaccard_sqrt <- sqrt(dist_jaccard)
dist_simpson_sqrt <- sqrt(dist_simpson)
dist_richness_sqrt <- sqrt(dist_richness)

# 7. PERMDISP: testing variance
disp_jaccard <- betadisper(dist_jaccard, df_agg$Trees)
disp_simpson <- betadisper(dist_simpson, df_agg$Trees)


print("--- PERMDISP Results ---")
print(permutest(disp_jaccard, permutations = 999))
print(permutest(disp_simpson, permutations = 999))

# 8. PERMANOVA
# Overall compositional dissimilarity (Total beta-diversity)
perm_jaccard <- adonis2(dist_jaccard_sqrt ~ Trees + Altitude_scaled + Exposition2 + Year + Month, 
                        data = df_agg, 
                        permutations = 999,
                        by = "margin")
# Species turnover (Simpson index)
perm_simpson <- adonis2(dist_simpson_sqrt ~ Trees + Altitude_scaled + Exposition2 + Year + Month, 
                        data = df_agg, 
                        permutations = 999,
                        by = "margin")
# Species richness uniformity
perm_richness <- adonis2(dist_richness_sqrt ~ Trees + Altitude_scaled + Exposition2 + Year + Month, 
                         data = df_agg, 
                         permutations = 999,
                         by = "margin")
print("--- PERMANOVA of Elevational gradient, Trees, and Time ---")
print(perm_jaccard)
print(perm_simpson)
print(perm_richness)
