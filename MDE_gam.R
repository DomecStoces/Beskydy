library(readxl)
library(writexl)
library(dplyr)
library(mgcv)
library(ggplot2)
library(tidyr)
df <- read_excel("Beskydy_long.xlsx", sheet = "Sheet1")
df$Altitude_scaled <- as.numeric(scale(df$Altitude, center = TRUE, scale = TRUE))
df$Locality <- as.factor(df$Locality)
df$Trees <- as.factor(df$Trees)
df$Year <- as.factor(df$Year)
df$Functional.group <- as.factor(df$Functional.group)

model2 <- gam(Count ~ Functional.group + s(Altitude_scaled) + s(Altitude_scaled, Functional.group, bs = "fs") + s(Locality, bs = "re") + Year,
  data = df,
  family = nb(link = "log"),
  method = "REML"
)
summary(model2)
par(mfrow = c(2, 2))
gam.check(model2)
concurvity(model2, full = TRUE)
gratia::draw(model2)
plot(model2, select = 2)

# Define the groups we are analyzing (Now including Herbivore)
target_groups <- c("Detritivore", "Predator", "Herbivore")

# ---------------------------------------------------------
# STEP 1: Predict the OBSERVED GAM curves
# ---------------------------------------------------------
cat("Step 1: Calculating observed GAM predictions...\n")

# Create a smooth grid of altitudes to draw the lines on
alt_grid <- seq(min(df$Altitude_scaled, na.rm = TRUE), 
                max(df$Altitude_scaled, na.rm = TRUE), 
                length.out = 100)

# Set up dummy data to predict on for the target groups
obs_newdata <- expand.grid(
  Altitude_scaled = alt_grid,
  Functional.group = target_groups,
  # We mathematically exclude random effects, so dummy levels are fine
  Locality = df$Locality[1], 
  Year = df$Year[1]          
)

# Predict on the link (log) scale to calculate proper confidence intervals
obs_pred <- predict(model2, newdata = obs_newdata, 
                    exclude = c("s(Locality)", "s(Year)"), # Ignore spatial/temporal random effects
                    se.fit = TRUE, type = "link")

# Convert back to the count scale (exponentiate)
obs_newdata$Obs_Fit <- exp(obs_pred$fit)
obs_newdata$Obs_LCI <- exp(obs_pred$fit - (1.96 * obs_pred$se.fit))
obs_newdata$Obs_UCI <- exp(obs_pred$fit + (1.96 * obs_pred$se.fit))

# ---------------------------------------------------------
# STEP 2: Simulate the NULL MODEL (Randomization)
# ---------------------------------------------------------
n_sims <- 999
null_preds_list <- list()

# Filter data just to the three target groups
df_sub <- df %>% filter(Functional.group %in% target_groups)

cat("Step 2: Running null model simulations (n =", n_sims, "). This may take a moment...\n")

for (i in 1:n_sims) {
  
  # Shuffle Altitude independently within each functional group
  df_null <- df_sub %>%
    group_by(Functional.group) %>%
    mutate(Altitude_scaled_null = sample(Altitude_scaled)) %>%
    ungroup()
  
  # Fit a null GAM (Note: for speed in loops, we use the simpler gam call)
  null_model <- gam(
    Count ~ Functional.group +
      s(Altitude_scaled_null, k = 12) +
      s(Altitude_scaled_null, by = Functional.group, k = 12),
    data = df_null,
    family = nb(link = "log")
  )
  
  # Predict the null curve
  null_newdata <- expand.grid(Altitude_scaled_null = alt_grid, 
                              Functional.group = target_groups)
  
  preds <- predict(null_model, newdata = null_newdata, type = "response")
  
  # Save the results
  null_preds_list[[i]] <- data.frame(Altitude_scaled = alt_grid, 
                                     Functional.group = null_newdata$Functional.group, 
                                     Sim = i, 
                                     Null_Fit = preds)
}

# ---------------------------------------------------------
# STEP 3: Summarize the Null Model
# ---------------------------------------------------------
cat("Step 3: Summarizing results...\n")

# Calculate the mean and 95% confidence interval of the 100 randomizations
null_summary <- bind_rows(null_preds_list) %>%
  group_by(Functional.group, Altitude_scaled) %>%
  summarize(
    Null_Mean = mean(Null_Fit),
    Null_LCI = quantile(Null_Fit, 0.025),  # Lower boundary of random chance
    Null_UCI = quantile(Null_Fit, 0.975),  # Upper boundary of random chance
    .groups = "drop"
  )

# Combine observed ecological data and null expectation data for plotting
plot_data <- left_join(obs_newdata, null_summary, by = c("Altitude_scaled", "Functional.group"))

# ---------------------------------------------------------
# STEP 4: Visualize with ggplot2 (Publication ready)
# ---------------------------------------------------------
cat("Step 4: Generating ggplot...\n")

ggplot(plot_data, aes(x = Altitude_scaled)) +
  
  # 1. The Null Model Expectation (Grey Ribbon) - MDE
  geom_ribbon(aes(ymin = Null_LCI, ymax = Null_UCI, fill = "Null Expectation (95% CI)"), alpha = 0.5) +
  geom_line(aes(y = Null_Mean), color = "grey40", linetype = "dashed", size = 0.8) +
  
  # 2. The Observed Ecological Data (Colored Ribbons and Lines)
  geom_ribbon(aes(ymin = Obs_LCI, ymax = Obs_UCI, fill = "Observed GAM (95% CI)"), alpha = 0.3) +
  geom_line(aes(y = Obs_Fit, color = Functional.group), size = 1.2) +
  
  # Formatting
  facet_wrap(~ Functional.group, scales = "free_y", nrow = 1) + # Set scales to free to see shapes clearly
  theme_bw(base_size = 14) + 
  scale_fill_manual(values = c("Null Expectation (95% CI)" = "grey60", 
                               "Observed GAM (95% CI)" = "black")) + 
  scale_color_manual(values = c("Detritivore" = "#7570b3",
                                "Predator" = "#d95f02",   
                                "Herbivore" = "#1b9e77")) +
  labs(
    x = "Elevational gradient (scaled)",
    y = "Predicted number of individuals",
    fill = "Uncertainty",
    color = "Functional group"
  ) +
  theme(legend.position = "right",
        strip.text = element_text(face = "bold", size = 14),
        panel.grid.minor = element_blank())

# Species richness #
Richness_df <- df %>%
  filter(Count > 0) %>% 
  group_by(Locality, Year, Altitude_scaled, Functional.group) %>%
  summarize(Richness = n_distinct(Species_Name), .groups = "drop") %>%
  complete(
    nesting(Locality, Year, Altitude_scaled), 
    Functional.group,                         
    fill = list(Richness = 0)                 
  )
Richness_df$Locality <- as.factor(Richness_df$Locality)
Richness_df$Year <- as.factor(Richness_df$Year)
Richness_df$Functional.group <- as.factor(Richness_df$Functional.group)
model_richness <- gam(
  Richness ~ Functional.group + 
    s(Altitude_scaled, by = Functional.group, k = 12,bs = "fs") +  
    s(Locality, bs = "re") + 
    Year,
  data = Richness_df,
  family = nb(link = "log"),
  method = "REML"
)
# Check the new summary
summary(model_richness)
par(mfrow = c(2, 2))
gam.check(model_richness)
concurvity(model_richness, full = TRUE)
gratia::draw(model_richness)
plot(model_richness, select = 2)
# 3. Predict and Plot
target_groups <- c("Predator", "Saproxylic")

alt_grid <- seq(min(Richness_df$Altitude_scaled, na.rm = TRUE),
                max(Richness_df$Altitude_scaled, na.rm = TRUE),
                length.out = 100)

obs_newdata <- expand.grid(
  Altitude_scaled = alt_grid,
  Functional.group = target_groups,
  Locality = Richness_df$Locality[1],
  Year = Richness_df$Year[1]
)

obs_pred <- predict(model_richness,
                    newdata = obs_newdata,
                    type = "link",
                    se.fit = TRUE,
                    exclude = "s(Locality)")

obs_newdata$Obs_Fit <- exp(obs_pred$fit)
obs_newdata$Obs_LCI <- exp(obs_pred$fit - 1.96 * obs_pred$se.fit)
obs_newdata$Obs_UCI <- exp(obs_pred$fit + 1.96 * obs_pred$se.fit)

n_sims <- 999
null_preds_list <- list()

Richness_sub <- Richness_df %>%
  filter(Functional.group %in% target_groups)

for (i in 1:n_sims) {
  
  # Shuffle altitude WITHIN functional groups
  df_null <- Richness_sub %>%
    group_by(Functional.group) %>%
    mutate(Altitude_scaled_null = sample(Altitude_scaled)) %>%
    ungroup()
  
  # Fit null GAM
  null_model <- gam(
    Richness ~ Functional.group +
      s(Altitude_scaled_null, k = 12) +
      s(Altitude_scaled_null, by = Functional.group, k = 12),
    data = df_null,
    family = nb(link = "log")
  )
  
  # Predict
  null_newdata <- expand.grid(
    Altitude_scaled_null = alt_grid,
    Functional.group = target_groups
  )
  
  preds <- predict(null_model, newdata = null_newdata, type = "response")
  
  null_preds_list[[i]] <- data.frame(
    Altitude_scaled = alt_grid,
    Functional.group = null_newdata$Functional.group,
    Sim = i,
    Null_Fit = preds
  )
}
null_summary <- bind_rows(null_preds_list) %>%
  group_by(Functional.group, Altitude_scaled) %>%
  summarize(
    Null_Mean = mean(Null_Fit),
    Null_LCI = quantile(Null_Fit, 0.025),
    Null_UCI = quantile(Null_Fit, 0.975),
    .groups = "drop"
  )
plot_data <- left_join(
  obs_newdata,
  null_summary,
  by = c("Altitude_scaled", "Functional.group")
)
ggplot(plot_data, aes(x = Altitude_scaled)) +
  geom_ribbon(aes(ymin = Null_LCI, ymax = Null_UCI,
                  fill = "Null Expectation (95% CI)"),
              alpha = 0.5) +
  geom_line(aes(y = Null_Mean),
            color = "grey40", linetype = "dashed", size = 0.8) +
  
  # OBSERVED
  geom_ribbon(aes(ymin = Obs_LCI, ymax = Obs_UCI,
                  fill = "Observed GAM (95% CI)"),
              alpha = 0.3) +
  geom_line(aes(y = Obs_Fit, color = Functional.group),
            size = 1.2) +
  
  facet_wrap(~ Functional.group, scales = "free_y") +
  
  theme_bw(base_size = 14) +
  scale_fill_manual(values = c(
    "Null Expectation (95% CI)" = "grey70",
    "Observed GAM (95% CI)" = "black"
  )) +
  scale_color_manual(values = c("Predator" = "#d95f02",   
                                "Saproxylic" = "#00A9FF")) +
  
  labs(
    x = "Elevational gradient (scaled)",
    y = "Predicted species richness",
    fill = "Uncertainty",                
    color = "Functional group"
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold"),
    panel.grid.minor = element_blank()
  )