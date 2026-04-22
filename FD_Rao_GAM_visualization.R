# 1. Define the spider color to match the others
spider_color <- "#D55E00" 

# --- Fix for new_data: added 'Trees' and updated 'Year' to 'Time.period' ---
new_data <- data.frame(
  Altitude_scaled = seq(min(df$Altitude_scaled, na.rm = TRUE), 
                        max(df$Altitude_scaled, na.rm = TRUE), 
                        length.out = 100),
  Exposition2     = mean(df$Exposition2, na.rm = TRUE),
  Site.protection = df$Site.protection[1], 
  Time.period     = df$Time.period[1],            
  Trees           = df$Trees[1],                 
  Locality        = df$Locality[1]                 
)

# 2. Predict on the "link" (log) scale
# (Note: If you want to exclude Time.period from the global trend as well, 
# you can change this to exclude = c("s(Locality)", "s(Time.period)"))
preds <- predict(mod_gam2, 
                 newdata = new_data, 
                 type = "link", 
                 se.fit = TRUE,
                 exclude = "s(Locality)")

# 3. Calculate limits on the log scale, then back-transform using exp()
fv <- new_data
fv$fitted <- exp(preds$fit)
fv$lower  <- exp(preds$fit - (1.96 * preds$se.fit))
fv$upper  <- exp(preds$fit + (1.96 * preds$se.fit))

# 4. Plot
p <- ggplot() +
  # The confidence ribbon (styled with spider_color)
  geom_ribbon(data = fv,
              aes(x = Altitude_scaled, ymin = lower, ymax = upper),
              fill = spider_color, alpha = 0.35) +
  
  # The fitted global trend line (styled with spider_color)
  geom_line(data = fv,
            aes(x = Altitude_scaled, y = fitted), 
            color = spider_color, linewidth = 1.1) +
  
  # The raw data points (updated to triangles, larger size, and spider_color)
  geom_jitter(data = df,
              aes(x = Altitude_scaled, y = Rao),
              width = 0.03, height = 0, 
              size = 2.5, shape = 16,       
              color = spider_color, alpha = 0.8) +
  
  # Labels
  labs(x = "Elevational gradient (scaled)", 
       y = "Functional diversity (Rao's Q)") +
  
  # Scales
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  scale_y_continuous(
    limits = c(0, 0.5),                      
    breaks = seq(0, 0.5, by = 0.1),          
    expand = expansion(mult = c(0.05, 0.05)) 
  ) +
  
  # Unified Theme
  theme_bw() +
  theme(
    axis.title        = element_text(size = 15),
    axis.text         = element_text(colour = "black", size = 14),
    plot.margin       = margin(6, 8, 6, 6)
  )
p
