# Number of species for each row # 
library(readxl)
library(writexl)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_compo_names")
df$species_richness <- rowSums(df[, -1] > 0)
write_xlsx(df[, c("ID", "species_richness")], "species_richness.xlsx")

library(vegan)
library(dplyr)
library(tidyr)
library(ggplot2)

df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "spiders_FD")
df$Altitude_scaled <- as.numeric(scale(df$Altitude, center = TRUE, scale = TRUE))
df$Locality <- as.factor(df$Locality)
df$Trees <- as.factor(df$Trees)
df$Year <- as.factor(df$Year)
compo_names <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "spiders_compo_names")

# 2. Prepare Community Matrix
comm_matrix <- as.matrix(compo_names[, -1])
rownames(comm_matrix) <- compo_names$ID

# Convert to presence/absence (1/0)
comm_pa <- ifelse(comm_matrix > 0, 1, 0)

# 3. Calculate Independent Beta-Diversity Components
dist_jaccard <- designdist(comm_pa, method = "1 - (J / (A + B - J))", terms = "binary")
dist_simpson <- designdist(comm_pa, method = "1 - (J / pmin(A, B))", terms = "binary")
dist_richness <- designdist(comm_pa, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary")

# Square-root transform them
dist_jaccard_sqrt  <- sqrt(dist_jaccard)
dist_simpson_sqrt  <- sqrt(dist_simpson)
dist_richness_sqrt <- sqrt(dist_richness)

# 4. Prepare Metadata
# Notice: You overwrote your nice Date/Month cleaning in your script! 
# Let's keep it simple and just use 'df' directly since rows match 'compo_names'
metadata_matched <- df
metadata_matched$ID <- 1:nrow(metadata_matched)

# Filter the metadata to match exactly with species data
# (Fixed the typo here: used compo_names$ID instead of spiders_compo_names$ID)
metadata_matched <- metadata_matched[metadata_matched$ID %in% compo_names$ID, ]

# Scale the Altitude variable and convert categorical variables to factors
metadata_matched$Altitude_scaled <- scale(metadata_matched$Altitude)
metadata_matched$Locality <- as.factor(metadata_matched$Locality)
metadata_matched$Trees <- as.factor(metadata_matched$Trees)

# 5. TESTING BETA-DIVERSITY

# A. Testing CATEGORICAL variables (Dispersion / Variance)
disp_jaccard <- betadisper(dist_jaccard_sqrt, metadata_matched$Trees)
disp_simpson <- betadisper(dist_simpson_sqrt, metadata_matched$Trees)
print("--- PERMDISP Results ---")
print(permutest(disp_jaccard, permutations = 999))
print(permutest(disp_simpson, permutations = 999))

# B. Testing CONTINUOUS variables (Compositional Shift) -> adonis2 (PERMANOVA)
# This asks: Does the species composition shift along the altitude gradient?
perm_control <- how(plots = Plots(strata = metadata_matched$Locality, type = "free"),
                    within = Within(type = "none"),
                    nperm = 999)
perm_jaccard <- adonis2(dist_jaccard_sqrt ~ Trees + Altitude_scaled, 
                        data = metadata_matched, 
                        permutations = perm_control)
perm_simpson <- adonis2(dist_simpson_sqrt ~ Trees + Altitude_scaled, 
                        data = metadata_matched, 
                        permutations = perm_control)
perm_richness <- adonis2(dist_richness_sqrt ~ Trees + Altitude_scaled, 
                         data = metadata_matched, 
                         permutations = perm_control)

print("--- PERMANOVA of Elevational gradient shuffling whole Locality ---")
print(perm_jaccard)
print(perm_simpson)
print(perm_richness)

# Post-hoc testy
TukeyHSD(disp_turnover)
TukeyHSD(disp_total)

# 4. Extrakce vzdáleností a příprava pro plot
df_turnover <- data.frame(Distance = disp_turnover$distances,
                          Treatment = metadata$Treatment,
                          Component = "Turnover")
df_nested <- data.frame(Distance = disp_nested$distances,
                        Treatment = metadata$Treatment,
                        Component = "Nestedness")
df_total <- data.frame(Distance = disp_total$distances,
                       Treatment = metadata$Treatment,
                       Component = "β-diversity (Jaccard)")

plot_data_box <- bind_rows(df_nested, df_turnover, df_total)
plot_data_box$Component <- factor(plot_data_box$Component, 
                                  levels = c("Nestedness", "Turnover", "β-diversity (Jaccard)"))

# 5. Zde bys použil svou funkci get_sig_letters() pro výpočet signifikance
# ... tvůj kód z dotazu ...

# 6. Boxploty
# Poznámka: 'ann_text' a Y-pozici pro 'Distance' si případně uprav podle skutečných p-hodnot!
bw_colors <- c("EKOLOGIE" = "white", 
               "KONVENCE" = "grey75", 
               "REGENERACE" = "grey40")

d4 <- ggplot(plot_data_box, aes(x = Treatment, y = Distance, fill = Treatment)) +
  geom_boxplot(color = "black", outlier.shape = 16, outlier.size = 1.5, alpha = 0.9) +
  # Odkomentuj následující řádek, až budeš mít připravený ann_text data frame
  # geom_text(data = ann_text, aes(label = label), vjust = -0.5, size = 5, color = "black") +
  facet_wrap(~Component, scales = "free_y") +
  scale_fill_manual(values = bw_colors) +
  theme_bw(base_size = 15) +
  theme(panel.grid.major.x = element_blank(),
        panel.grid.minor = element_blank(),
        strip.background = element_blank(),
        strip.text = element_text(face = "plain", size = 14, color = "black"),
        axis.text.x = element_text(angle = 45, hjust = 1, color = "black"),
        axis.text.y = element_text(color = "black"),
        axis.title = element_text(color = "black"),
        legend.position = "none") +
  labs(x = "Treatment", y = "Distance to group centroid")

print(d4)