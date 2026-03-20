# Number of species for each row # 
library(readxl)
library(writexl)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_compo_names")
df$species_richness <- rowSums(df[, -1] > 0)
write_xlsx(df[, c("ID", "species_richness")], "species_richness.xlsx")

library(vegan)
library(dplyr)
library(ggplot2)



# 1. Převod na presence/absence
comm_pa <- ifelse(comm_matrix > 0, 1, 0)

# 2. Výpočet NEZÁVISLÝCH indexů pomocí designdist()
# A. Jaccard (Náhrada / Celková disimilarita)
# Vzorec disimilarity: 1 - (J / (A + B - J))
dist_jaccard <- designdist(comm_pa, method = "1 - (J / (A + B - J))", terms = "binary")

# B. Simpson (Podle textu pro 'Nestness' / překryv)
# Vzorec disimilarity: 1 - (J / min(A, B))
dist_simpson <- designdist(comm_pa, method = "1 - (J / pmin(A, B))", terms = "binary")

# C. Uniformita species richness (Index R)
# Text definuje R = min(Sx, Sy) / max(Sx, Sy). 
# Pro disimilaritu (vzdálenost pro PERMDISP) použijeme 1 - R:
dist_richness_ratio <- designdist(comm_pa, method = "1 - (pmin(A, B) / pmax(A, B))", terms = "binary")

# 3. Transformace pro PERMDISP (aby nevznikaly negativní vlastní čísla v PCoA)
dist_jaccard_sqrt <- sqrt(dist_jaccard)
dist_simpson_sqrt <- sqrt(dist_simpson)
dist_richness_sqrt <- sqrt(dist_richness_ratio)

# 4. PERMDISP (Tady už pokračuješ standardně jako předtím)
disp_jaccard <- betadisper(dist_jaccard_sqrt, metadata$Treatment)
disp_simpson <- betadisper(dist_simpson_sqrt, metadata$Treatment)
disp_richness <- betadisper(dist_richness_sqrt, metadata$Treatment)

# Globální testy
perm_turnover <- permutest(disp_turnover, permutations = 999)
perm_nested   <- permutest(disp_nested, permutations = 999)
perm_total    <- permutest(disp_total, permutations = 999)

print(perm_turnover)
print(perm_nested)
print(perm_total)

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