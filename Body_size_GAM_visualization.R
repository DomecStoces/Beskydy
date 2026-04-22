library(gratia)
library(dplyr)
library(tidyr)
library(ggplot2)

# vizualization for Size #
excl <- c("s(Locality)")
tv <- typical_values(mod_gam2)
alt_seq <- seq(min(df$Altitude_scaled, na.rm = TRUE),
               max(df$Altitude_scaled, na.rm = TRUE), length.out = 100)

tv2 <- dplyr::select(tv, -any_of(c("Altitude_scaled","Altitude_scaled2")))
new_data <- tidyr::crossing(tv2, tibble(Altitude_scaled = alt_seq))
fv <- fitted_values(mod_gam2, data = new_data, exclude = excl,
                    scale = "link", se = TRUE) %>%
  dplyr::rename(fitted_link = any_of(c("fitted",".fitted","fit")),
                se_link     = any_of(c("se",".se"))) %>%
  mutate(
    lower_link = fitted_link - (1.96 * se_link),
    upper_link = fitted_link + (1.96 * se_link),
    fitted = exp(fitted_link),  
    lower  = exp(lower_link),   
    upper  = exp(upper_link)    
  )
spider_color <- "#D55E00"
weevil_color <- "tan4"
chilo_color <-"#0072B2"

p <- ggplot() +
  # The confidence ribbon
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = chilo_color, alpha = 0.3) +
  
  # The fitted line
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), 
            color = chilo_color, linewidth = 1.1) +
  
  # The points (using actual Size)
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Size),
              width = 0.03, height = 0, size = 2.5, alpha = 0.8,
              shape = 15, color = chilo_color) +
  
  # Labels
  labs(x = "Elevational gradient (scaled)", y = "Body size CWM") +
  
  # Closed the parenthesis right after the rounding function
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) + 
  
  # CHANGED to let ggplot auto-scale to your 3-8.5 size range
  scale_y_continuous(expand = expansion(mult = c(0.05, 0.05))) +
  
  theme_bw() +
  theme(
    axis.title        = element_text(size = 15),
    axis.text         = element_text(colour = "black", size = 14),
    plot.margin       = margin(6, 8, 6, 6)
  )

p
