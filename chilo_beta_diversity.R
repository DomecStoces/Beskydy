# 1. Načtení dat pro půdní faunu (DCI)
# Zkontroluj, že názvy sheetů přesně odpovídají tvému Excelu!
df_dci <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_FD")
compo_names_dci <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_compo_names")

# 2. Spárování metadat
metadata_dci <- df_dci
metadata_dci$ID <- 1:nrow(metadata_dci)
metadata_matched_dci <- metadata_dci[metadata_dci$ID %in% compo_names_dci$ID, ]

# 3. Příprava surové druhové matice
comm_matrix_dci <- as.matrix(compo_names_dci[, -1])
rownames(comm_matrix_dci) <- compo_names_dci$ID

# 4. AGREGACE (s pojistkou drop_na)
df_combined_dci <- bind_cols(metadata_matched_dci, as.data.frame(comm_matrix_dci))

df_agg_dci <- df_combined_dci %>%
  drop_na(Trees, Altitude) %>%
  group_by(Locality, Trees, Altitude) %>%
  summarise(across(all_of(colnames(comm_matrix_dci)), sum), .groups = "drop") %>%
  mutate(
    Altitude_scaled = scale(Altitude),
    Trees = as.factor(Trees),
    Locality = as.factor(Locality)
  )

# 5. Agregovaná matice a Presence/Absence
comm_agg_dci <- as.matrix(df_agg_dci %>% select(-Locality, -Trees, -Altitude, -Altitude_scaled))
rownames(comm_agg_dci) <- df_agg_dci$Locality
comm_pa_agg_dci <- ifelse(comm_agg_dci > 0, 1, 0)

# 6. Výpočet NOVÝCH matic vzdáleností speciálně pro DCI
dist_jaccard_dci <- designdist(comm_pa_agg_dci, method = "1 - (J / (A + B - J))", terms = "binary")
dist_simpson_dci <- designdist(comm_pa_agg_dci, method = "1 - (J / pmin(A, B))", terms = "binary")
dist_richness_dci <- designdist(comm_pa_agg_dci, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary")

dist_jaccard_sqrt_dci  <- sqrt(dist_jaccard_dci)
dist_simpson_sqrt_dci  <- sqrt(dist_simpson_dci)
dist_richness_sqrt_dci <- sqrt(dist_richness_dci)

# 7. KONEČNĚ TA SPRÁVNÁ PERMANOVA
perm_jaccard_dci <- adonis2(dist_jaccard_sqrt_dci ~ Trees + Altitude_scaled, 
                            data = df_agg_dci, permutations = 999, by = "margin")

perm_simpson_dci <- adonis2(dist_simpson_sqrt_dci ~ Trees + Altitude_scaled, 
                            data = df_agg_dci, permutations = 999, by = "margin")

perm_richness_dci <- adonis2(dist_richness_sqrt_dci ~ Trees + Altitude_scaled, 
                             data = df_agg_dci, permutations = 999, by = "margin")
print(perm_jaccard_dci)
print(perm_simpson_dci)
print(perm_richness_dci)

