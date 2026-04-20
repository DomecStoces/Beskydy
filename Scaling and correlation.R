site_env <- df %>%
  group_by(Locality) %>%
  summarise(
    Altitude    = mean(Altitude, na.rm = TRUE),
    Exposition2 = first(na.omit(Exposition2)), 
    HR          = first(na.omit(HR)),
    .groups = "drop"
  ) %>%
  mutate(
    Altitude_scaled  = as.numeric(scale(Altitude, center = TRUE, scale = TRUE)))
df <- df %>%
  left_join(site_env, by = "Locality")

# Correlation among CWMs 
library(corrplot)
cwm_mat3 <- df[, c("Wings_cwm", "Body_size_cwm", "Dietary_cwm")]
cor_mat <- cor(
  df[, c("Wings_cwm", "Body_size_cwm", "Dietary_cwm")],
  method = "spearman",
  use = "pairwise.complete.obs"
)
colnames(cor_mat) <- rownames(cor_mat) <- c(
  "Dispersal ability",
  "Body size",
  "Trophic strategy"
)
corrplot(
  cor_mat,
  method = "color",
  tl.col = "black",
  tl.cex = 1.2,
  addCoef.col = "black",
  number.cex = 1.2,
  col = colorRampPalette(c("#2166AC","#FFFFFF","#B2182B"))(200)
)

# or PCA
library(FactoMineR)
library(factoextra)
cwm_mat3 <- df %>%
  select(
    `Dispersal ability`   = Wings_cwm,
    `Body size` = Body_size_cwm,
    `Trophic strategy`            = Dietary_cwm
  ) %>%
  na.omit()
res.pca3 <- PCA(cwm_mat3, scale.unit = TRUE, graph = FALSE)
fviz_pca_var(
  res.pca3,
  repel  = TRUE,
  col.var = "black"
)

loadings <- res.pca3$var$coord
loadings
# How CWMs jointly respond to environment?
# the overall trait–environment concordance
library(vegan)
rda_cwm <- rda(cwm_mat ~ Altitude + Exposition2, data = cwm_clean)
anova(rda_cwm, permutations = 999)
anova(rda_cwm, by = "axis", permutations = 999)

tiff('PCA.tiff', units="in", width=7, height=6, res=600)
fviz_pca_var(
  res.pca3,
  repel  = TRUE,
  col.var = "black"
)
dev.off()

# Correlation between precipitation and temperature #
library(corrplot)
library(dplyr)
df1 <- read_excel("CANOCO_FINAL.xlsx", sheet = "env")
# Select only the numeric environmental variables
numeric_vars <- c("Temperature", "Precipitation")

# Compute Spearman correlation matrix
cor_matrix <- df1 %>% select(all_of(numeric_vars)) %>% cor(method = "spearman", use = "pairwise.complete.obs")

# Plot correlation heatmap
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8)

# Save to TIFF
tiff("Spearman_rank_corr.tiff", units = "in", width = 7, height = 5, res = 300)
corrplot(cor_matrix, method = "color", type = "upper",
         order = "hclust", tl.col = "black", tl.srt = 45,
         addCoef.col = "black", number.cex = 0.8)
dev.off()

# Quantitative check of multicollinearity using VIF
# A VIF value above 5 or 10 indicates problematic multicollinearity
library(car)
model_vif <- lm(Altitude ~ Precipitation + Temperature, data = df1)
vif(model_vif)
