### Setting model ###
library(readxl)
library(mgcv)
library(stringr)
library(dplyr)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "spiders_FD")
df$Altitude_scaled <- as.numeric(scale(df$Altitude, center = TRUE, scale = TRUE))
df$Locality <- as.factor(df$Locality)
df$Trees <- as.factor(df$Trees)
df$Time.period <- as.factor(df$Time.period)
df <- df %>%
  mutate(
    Exposition2 = sapply(strsplit(as.character(Exposition), "_"), function(x) mean(as.numeric(x))),
    Exposition2 = as.numeric(scale(Exposition2))
  )
head(df %>% select(Exposition, Exposition2))

# Scaling traits for spiders #
n <- nrow(df)
df$Trophic_01 <- df$Trophic - 1
df$Trophic_scaled <- (df$Trophic_01 * (n - 1) + 0.5) / n
df$Dispersal_scaled <- (df$Dispersal * (n - 1) + 0.5) / n

# Scaling traits for carabids #
min_val <- min(df$Trophic, na.rm = TRUE)
max_val <- max(df$Trophic, na.rm = TRUE)

df$Trophic_norm <- (df$Trophic - min_val) / (max_val - min_val)
n <- nrow(df)
df$Trophic_scaled <- (df$Trophic_norm * (n - 1) + 0.5) / n
df$Dispersal_scaled <- (df$Dispersal * (n - 1) + 0.5) / n

# Scaling traits for weevils #
n <- nrow(df)
df$Dispersal_01 <- df$Dispersal - 1
df$Dispersal_scaled <- (df$Dispersal_01 * (n - 1) + 0.5) / n

# Scaling traits for chilo-diplo-iso #
n <- nrow(df)
df$Trophic_01 <- df$Trophic - 1
df$Trophic_scaled <- (df$Trophic_01 * (n - 1) + 0.5) / n

mod_gam2 <- gam(
  Size ~ s(Locality, bs = "re")+
    Altitude_scaled + Exposition2 + Site.protection + s(Time.period, bs = "re") + Trees,
  data   = df,
  family = gaussian(link="log"),
  method = "REML"
)

mod_gam2 <- gam(
  Trophic_scaled ~ s(Locality, bs = "re") +
    Altitude_scaled + Exposition2 + Site.protection + s(Time.period, bs = "re") + Trees,
  data   = df,
  family = betar(link="logit"),
  method = "REML"
)

mod_gam2 <- gam(
  Dispersal_scaled ~ s(Locality, bs = "re") +
    Altitude_scaled + Exposition2 + Site.protection + s(Time.period, bs = "re") + Trees,
  data   = df,
  family = betar(link="cloglog"),
  method = "REML"
)

mod_gam2 <- gam(
  Rao ~ 
    s(Locality, bs = "re") + 
    Altitude_scaled  + Exposition2 + Site.protection + s(Time.period, bs = "re") + Trees,
  data   = df,
  family = tw(link="log"), select = TRUE,
  method = "REML"
)

summary(mod_gam2)
par(mfrow = c(2, 2))
gam.check(mod_gam2)
concurvity(mod_gam2, full = TRUE)
gratia::draw(mod_gam2)
plot(mod_gam2, select = 2)