library(mgcv)

N <- nrow(df)
df$Wings_cwm_scaled        <- (df$Wings_cwm * (N - 1) + 0.5) / N
df$Dietary_cwm_scaled      <- (df$Dietary_cwm * (N - 1) + 0.5) / N
df$Breeding_cwm_scaled     <- (df$Breeding_cwm * (N - 1) + 0.5) / N
df$Distribution_cwm_scaled <- (df$Distribution_cwm * (N - 1) + 0.5) / N

df$Year <- factor(df$Year)
df$Locality <- factor(df$Locality)

mod_gam1 <- gam(
  Body_size_cwm ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr") + Exposition2 +
    s(Year, bs = "re"),
  data   = df,
  family = gaussian(link="log"),
  method = "REML"
)
mod_gam2 <- gam(
  Trophic_cwm ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr") + Exposition2 +
    s(Year, bs = "re"),
  data   = df,
  family = betar(link="cloglog"),
  method = "REML"
)
summary(mod_gam1)
par(mfrow = c(2, 2))
gam.check(mod_gam1)
concurvity(mod_gam1, full = TRUE)
gratia::draw(mod_gam1)
plot(mod_gam1, select = 2)