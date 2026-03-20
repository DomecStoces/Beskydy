# Number of species for each row # 
library(readxl)
library(writexl)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_compo_names")
df$species_richness <- rowSums(df[, -1] > 0)
write_xlsx(df[, c("ID", "species_richness")], "species_richness.xlsx")
