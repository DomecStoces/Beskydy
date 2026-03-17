### Setting model ###
library(readxl)
library(mgcv)
library(stringr)
library(dplyr)
df <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "spiders_FD")
df$Altitude_scaled <- as.numeric(scale(df$Altitude, center = TRUE, scale = TRUE))
df$Locality <- as.factor(df$Locality)
df$Trees <- as.factor(df$Trees)
df$Year <- as.factor(df$Year)
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


mod_gam1 <- gam(
  Size ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr",k=3) + Exposition2 +
    Year,
  data   = df,
  family = gaussian(link="log"),
  method = "REML"
)

mod_gam2 <- gam(
  Size ~ s(Locality, bs = "re") +
    Altitude_scaled + Exposition2 + Site.protection + Year,
  data   = df,
  family = gaussian(link="log"),
  method = "REML"
)

mod_gam2 <- gam(
  Rao ~ 
    s(Locality, bs = "re") + 
    Altitude_scaled  + Exposition2 + Site.protection + Year,
  data   = df,
  family = tw(link="log"), select = TRUE,
  method = "REML"
)

summary(mod_gam2)
par(mfrow = c(2, 2))
gam.check(mod_gam2)
concurvity(mod_gam2, full = TRUE)
gratia::draw(mod_gam2)
plot(mod_gam1, select = 2)

### Plotting the effect of Altitude_scaled on CWM traits ###
library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)

excl <- c("s(Locality)")
tv <- typical_values(mod_gam2)

# 1. Create the smooth sequence of 100 points
alt_seq <- seq(min(df$Altitude_scaled, na.rm = TRUE),
               max(df$Altitude_scaled, na.rm = TRUE), length.out = 100)

tv2 <- dplyr::select(tv, -any_of(c("Altitude_scaled","Altitude_scaled2")))

# 2. Build the prediction grid
new_data <- tidyr::crossing(tv2, tibble(Altitude_scaled = alt_seq))

# 3. Calculate fitted values USING new_data (not df!)
fv <- fitted_values(mod_gam2, data = new_data, exclude = excl,
                    scale = "link", se = TRUE) %>%
  dplyr::rename(fitted_link = any_of(c("fitted",".fitted","fit")),
                se_link     = any_of(c("se",".se"))) %>%
  mutate(
    lower_link = fitted_link - (1.96 * se_link),
    upper_link = fitted_link + (1.96 * se_link),
    fitted = plogis(fitted_link),  
    lower  = plogis(lower_link),   
    upper  = plogis(upper_link)    
  )

# 4. Plot
p <- ggplot() +
  # The confidence ribbon (now smooth!)
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.35) +
  
  # The fitted line (now smooth!)
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), linewidth = 1.1) +
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Size),
              width = 0.03, height = 0, size = 1.8, alpha = 0.6) +
  labs(title = "Spiders", x = "Elevational gradient (scaled)", y = "Size CWM") +
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  
  # Y-Axis strictly 0 to 1
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

# vizualization for Size
excl <- c("s(Locality)")
tv <- typical_values(mod_gam2)

# 1. Create the smooth sequence of 100 points
alt_seq <- seq(min(df$Altitude_scaled, na.rm = TRUE),
               max(df$Altitude_scaled, na.rm = TRUE), length.out = 100)

tv2 <- dplyr::select(tv, -any_of(c("Altitude_scaled","Altitude_scaled2")))

# 2. Build the prediction grid
new_data <- tidyr::crossing(tv2, tibble(Altitude_scaled = alt_seq))

# 3. Calculate fitted values USING new_data
fv <- fitted_values(mod_gam2, data = new_data, exclude = excl,
                    scale = "link", se = TRUE) %>%
  dplyr::rename(fitted_link = any_of(c("fitted",".fitted","fit")),
                se_link     = any_of(c("se",".se"))) %>%
  mutate(
    lower_link = fitted_link - (1.96 * se_link),
    upper_link = fitted_link + (1.96 * se_link),
    fitted = exp(fitted_link),  # CHANGED: exp() for log-link
    lower  = exp(lower_link),   # CHANGED: exp() for log-link
    upper  = exp(upper_link)    # CHANGED: exp() for log-link
  )

# 4. Plot
p <- ggplot() +
  # The confidence ribbon
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = "grey70", alpha = 0.35) +
  
  # The fitted line
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), linewidth = 1.1) +
  
  # The points (using actual Size)
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Size),
              width = 0.03, height = 0, size = 1.8, alpha = 0.6) +
  
  # Labels
  labs(title = "Spiders", x = "Elevational gradient (scaled)", y = "Size CWM") +
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  
  # Y-Axis: CHANGED to let ggplot auto-scale to your 3-8.5 size range
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  
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

