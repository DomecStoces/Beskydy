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
spider_color <- "#D55E00"
# 2. Plot
p <- ggplot() +
  # The confidence ribbon (added the color here)
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = spider_color, alpha = 0.35) +
  
  # The fitted line (added the color here)
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), 
            color = spider_color, linewidth = 1.1) +
  
  # The points (added the color, triangles, larger size, and fixed the 'y' variable)
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Trophic_scaled),
              width = 0.03, height = 0, 
              size = 2.5,         
              shape = 16,        
              color = spider_color, alpha = 0.8) +
  # Labels
  labs(x = "Elevational gradient (scaled)", y = "Trophic strategy CWM") +
  # X-Axis
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  # Y-Axis strictly 0 to 1
  scale_y_continuous(
    limits = c(0, 1),              
    breaks = seq(0, 1, 0.2),         
    expand = expansion(mult = c(0, 0.02)) 
  ) +
  theme_bw() +
  theme(
    axis.title        = element_text(size = 15),
    axis.text         = element_text(colour = "black", size = 14),
    plot.margin       = margin(6, 8, 6, 6)
  )
p