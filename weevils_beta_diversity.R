# 1. Load WEEVIL Data
df_weevils <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "weevils_FD")
compo_names_weevils <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "weevils_compo_names")

# 2. Match metadata
metadata <- df_weevils
metadata$ID <- 1:nrow(metadata)
metadata_matched <- metadata[metadata$ID %in% compo_names_weevils$ID, ]

# 3. Prepare raw community matrix
comm_matrix <- as.matrix(compo_names_weevils[, -1])
rownames(comm_matrix) <- compo_names_weevils$ID

# 4. AGGREGATE DATA (With the crucial drop_na() to fix the error!)
df_combined <- bind_cols(metadata_matched, as.data.frame(comm_matrix))

df_agg_weevils <- df_combined %>%
  drop_na(Trees, Altitude) %>% # THIS FIXES THE WEEVIL ERROR
  group_by(Locality, Trees, Altitude) %>%
  summarise(across(all_of(colnames(comm_matrix)), sum), .groups = "drop") %>%
  mutate(
    Altitude_scaled = scale(Altitude),
    Trees = as.factor(Trees),
    Locality = as.factor(Locality)
  )

# 5. Prepare aggregated community matrix & Presence/Absence
comm_agg <- as.matrix(df_agg_weevils %>% select(-Locality, -Trees, -Altitude, -Altitude_scaled))
rownames(comm_agg) <- df_agg_weevils$Locality
comm_pa_agg <- ifelse(comm_agg > 0, 1, 0)

# 6. Calculate NEW distance matrices for Weevils
dist_jaccard <- designdist(comm_pa_agg, method = "1 - (J / (A + B - J))", terms = "binary")
dist_simpson <- designdist(comm_pa_agg, method = "1 - (J / pmin(A, B))", terms = "binary")
dist_richness <- designdist(comm_pa_agg, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary")

dist_jaccard_sqrt  <- sqrt(dist_jaccard)
dist_simpson_sqrt  <- sqrt(dist_simpson)
dist_richness_sqrt <- sqrt(dist_richness)

# 7. PERMANOVA for Weevils
perm_jaccard_weevils <- adonis2(dist_jaccard_sqrt ~ Trees + Altitude_scaled, 
                                data = df_agg_weevils, permutations = 999, by = "margin")

perm_simpson_weevils <- adonis2(dist_simpson_sqrt ~ Trees + Altitude_scaled, 
                                data = df_agg_weevils, permutations = 999, by = "margin")

perm_richness_weevils <- adonis2(dist_richness_sqrt ~ Trees + Altitude_scaled, 
                                 data = df_agg_weevils, permutations = 999, by = "margin")
print(perm_jaccard_weevils)
print(perm_simpson_weevils)
print(perm_richness_weevils)
