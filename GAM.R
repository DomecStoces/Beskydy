### Setting model ###
library(mgcv)
df <- read_excel("Rao_diversity1.xlsx", sheet = "RaoQ")
df$Altitude_scaled <- as.numeric(scale(df$Elevation, center = TRUE, scale = TRUE))
df$Locality <- as.factor(df$Locality)
df$Year <- as.factor(df$Year)
df$Exposition2 <- as.numeric(df$Exposition2)
df$Exposition2 <- scale(df$Exposition2)

N <- nrow(df)
df$Wings_cwm_scaled        <- (df$Wings_cwm * (N - 1) + 0.5) / N
df$Dietary_cwm_scaled      <- (df$Dietary_cwm * (N - 1) + 0.5) / N
df$Breeding_cwm_scaled     <- (df$Breeding_cwm * (N - 1) + 0.5) / N
df$Distribution_cwm_scaled <- (df$Distribution_cwm * (N - 1) + 0.5) / N

df$Year <- factor(df$Year)
df$Locality <- factor(df$Locality)

mod_gam1 <- gam(
  Body_size_cwm ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr",k=5) + Exposition2 +
    s(Year, bs = "re"),
  data   = df,
  family = gaussian(link="log"),
  method = "REML"
)
mod_gam2 <- gam(
  Trophic_cwm ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr",k=5) + Exposition2 +
    s(Year, bs = "re"),
  data   = df,
  family = betar(link="cloglog"),
  method = "REML"
)

mod_gam_rao <- gam(
  Q ~ 
    s(Locality, bs = "re") + 
    s(Altitude_scaled, bs = "cr", k = 5)  + Exposition2 +
    s(Year, bs = "re"),
  data   = df,
  family = tw(link="log"), select = TRUE,
  method = "REML"
)

summary(mod_gam1)
par(mfrow = c(2, 2))
gam.check(mod_gam1)
concurvity(mod_gam1, full = TRUE)
gratia::draw(mod_gam1)
plot(mod_gam1, select = 2)

### Plotting the effect of Altitude_scaled on CWM traits ###
library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)

excl <- c("s(Locality)", "s(Year)")
tv <- typical_values(mod_gam1)

alt_seq <- seq(min(df$Altitude_scaled, na.rm = TRUE),
               max(df$Altitude_scaled, na.rm = TRUE), length.out = 100)

tv2 <- dplyr::select(tv, -any_of(c("Altitude_scaled","Altitude_scaled2")))

new_data <- tidyr::crossing(tv2, tibble(Altitude_scaled = alt_seq)) %>%
  mutate(Altitude_scaled2 = Altitude_scaled^2)
new_data <- tidyr::crossing(tv2, tibble(Altitude_scaled = alt_seq)) %>%
  mutate(Altitude_scaled2 = Altitude_scaled^2)
inv_link <- family(mod_gam1)$linkinv
fv <- fitted_values(mod_gam1, data = new_data, exclude = excl,
                    scale = "link", se = TRUE) %>%
  dplyr::rename(fitted_link = any_of(c("fitted",".fitted","fit")),
                se_link     = any_of(c("se",".se"))) %>%
  mutate(
    lower_link = fitted_link - (1.96 * se_link),
    upper_link = fitted_link + (1.96 * se_link),
    fitted = inv_link(fitted_link),
    lower  = inv_link(lower_link),
    upper  = inv_link(upper_link)
  )
fv_obs <- fitted_values(mod_gam1, data = df, exclude = excl,
                        scale = "response", se = FALSE) %>%
  dplyr::rename(fitted = any_of(c("fitted",".fitted","fit")))

df$partial_excl <- residuals(mod_gam1, type = "response") + fv_obs$fitted

p <- ggplot() +
  # 1. The confidence ribbon
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.35) +
  
  # 2. The fitted line (the "estimated curve")
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), linewidth = 1.1) +
  
  # 3. The points (using your dispersal column)
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Wings_cwm_scaled),
              width = 0.03, height = 0, size = 1.8, alpha = 0.6) +
  
  # Labels
  labs(x = "Elevational gradient (scaled)", y = "Dispersal ability CWM") +
  
  # X-Axis: Matches the Rao's Q plot limits exactly
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  # Y-Axis: FORCED TO 0 - 1
  scale_y_continuous(
    limits = c(0, 1),             
    breaks = seq(0, 1, 0.2),         
    expand = expansion(mult = c(0, 0.02)) 
  ) +
  theme(
    panel.background = element_blank(),
    plot.background  = element_blank(),
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.line        = element_line(colour = "black", linewidth = 0.6),
    axis.ticks       = element_line(colour = "black", linewidth = 0.5),
    axis.ticks.length= unit(4, "pt"),
    axis.title       = element_text(size = 15),
    axis.text        = element_text(colour = "black", size = 11),
    plot.margin      = margin(6, 8, 6, 6)
  )
p
tiff('GAM_Distribution.tiff', units = "in", width = 8, height = 10, res = 600)
print(p)
dev.off()