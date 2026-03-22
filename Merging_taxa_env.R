library(readxl)
library(dplyr)
library(purrr)
library(writexl)

# File path
file_path <- "Beskydy_2007_2008_traits_final.xlsx"


# Define taxa and sheets
taxa_sheets <- list(
  spiders = c(env = "spiders_FD", compo = "spiders_compo_names"), 
  carabids = c(env = "carabids_FD", compo = "carabids_compo_names"),
  chilo_diplo_iso = c(env = "chilo_diplo_iso_FD", compo = "chilo_diplo_iso_compo_names"),
  weevils = c(env = "weevils_FD", compo = "weevils_compo_names")
)

# 4. Variables
fd_vars <- c("Size", "Trophic", "Dispersal", "Rao", 
             "Count", "Richness", "SESpd", "MPD")

env_vars <- c("Locality", "Year", "Time.period", 
              "Temperature", "Precipitation", 
              "Site.protection", "Altitude", "Trees")

# 5. Create SampleID
create_sample_id <- function(df) {
  df %>%
    mutate(
      Temperature = round(Temperature, 2),
      Precipitation = round(Precipitation, 2),
      SampleID = paste(Locality, Year, Time.period,
                       Temperature, Precipitation, Trees,
                       sep = "_")
    )
}

# 6. Process each taxon
process_taxon <- function(sheets, taxon_name) {
  
  df_env <- read_excel(file_path, sheet = sheets["env"])
  df_compo <- read_excel(file_path, sheet = sheets["compo"])
  df_env <- create_sample_id(df_env)
  if (!"ID" %in% colnames(df_env) | !"ID" %in% colnames(df_compo)) {
    stop(paste("ID missing in", taxon_name))
  }
  
  df_env <- df_env %>% mutate(ID = as.character(ID))
  df_compo <- df_compo %>% mutate(ID = as.character(ID))
  species_cols <- setdiff(colnames(df_compo), "ID")
  df_joined <- left_join(df_env, df_compo, by = "ID")
  df_joined <- df_joined %>%
    select(SampleID, all_of(env_vars), any_of(fd_vars), all_of(species_cols))
  df_joined <- df_joined %>%
    rename_with(~ paste0(taxon_name, "_", .), any_of(fd_vars))
  df_joined <- df_joined %>%
    rename_with(~ paste0(taxon_name, "_", .), all_of(species_cols))
  return(df_joined)
}

# 7. Apply function
processed_list <- imap(taxa_sheets, process_taxon)

# Optional check
map(processed_list, nrow)

# 8. Master environment
env_master <- processed_list[[1]] %>%
  select(SampleID, all_of(env_vars))

# 9. Extract taxa-only data
taxa_only <- map(processed_list, ~ select(.x, -all_of(env_vars)))

# 10. FULL JOIN across taxa
taxa_merged <- reduce(taxa_only, full_join, by = "SampleID")

# 11. Final dataset
master_df <- left_join(env_master, taxa_merged, by = "SampleID")

# 12. NA handling
# Identify biological columns (everything except env)
bio_cols <- setdiff(names(master_df), c("SampleID", env_vars))

master_df <- master_df %>%
  mutate(across(all_of(bio_cols), ~ replace(., is.na(.), 0)))

cat("Number of samples:", nrow(master_df), "\n")

if (any(duplicated(master_df$SampleID))) {
  warning("Duplicate SampleIDs detected!")
}
write_xlsx(master_df, "Beskydy_Master_Merged1.xlsx")

# Doing long format for functional groups #
library(tidyverse)
df <- read_excel("Beskydy_Master_Merged1.xlsx", sheet = "all")
Beskydy_Long <- df %>%
  pivot_longer(
    # Select all columns EXCEPT the first 8 metadata columns to be pivoted
    cols = -c(Locality, Year, Time.period, Temperature, Precipitation, Site.protection, Altitude, Trees),
    
    # Name the new column that will hold the column headers (the species names)
    names_to = "Species_Name",
    
    # Name the new column that will hold the numbers (the counts/abundances)
    values_to = "Count"
  )
Beskydy_Long <- Beskydy_Long %>%
  filter(Count > 0)
write_xlsx(Beskydy_Long, "Beskydy_long.xlsx")
