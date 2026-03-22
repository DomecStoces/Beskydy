library(readxl)
library(writexl)
library(dplyr)
library(mgcv)
library(ggplot2)
library(tidyr)
df <- read_excel("Beskydy_long.xlsx", sheet = "Sheet1")
df <- df %>%
  mutate(
    Altitude_scaled = as.numeric(scale(Altitude)),
    Locality = as.factor(Locality),
    Trees = as.factor(Trees),
    Year = as.factor(Year),
    Functional.group = as.factor(Functional.group)
  )


model2 <- gam(
  Count ~ Functional.group +
    s(Altitude_scaled, Functional.group, bs = "fs", k = 20) +
    s(Locality, bs = "re") +
    Year,
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
target_groups <- c("Detritivore", "Omnivore", "Herbivore","Saproxylic")

df_sub <- df %>% filter(Functional.group %in% target_groups)

# ---------------------------------------------------------
# STEP 1: OBSERVED GAM PREDICTIONS
# ---------------------------------------------------------
cat("Step 1: Observed GAM predictions...\n")

alt_grid <- seq(min(df$Altitude_scaled, na.rm = TRUE),
                max(df$Altitude_scaled, na.rm = TRUE),
                length.out = 100)

obs_newdata <- expand.grid(
  Altitude_scaled = alt_grid,
  Functional.group = target_groups,
  Locality = df$Locality[1],  # dummy (excluded later)
  Year = df$Year[1]           # fixed reference level
)

obs_pred <- predict(
  model2,
  newdata = obs_newdata,
  exclude = "s(Locality)",   # only random effect excluded
  se.fit = TRUE,
  type = "link"
)

obs_newdata <- obs_newdata %>%
  mutate(
    Obs_Fit = exp(obs_pred$fit),
    Obs_LCI = exp(obs_pred$fit - 1.96 * obs_pred$se.fit),
    Obs_UCI = exp(obs_pred$fit + 1.96 * obs_pred$se.fit)
  )

# ---------------------------------------------------------
# STEP 2: NULL MODEL (CORRECTED!)
# ---------------------------------------------------------
cat("Step 2: Running null model simulations...\n")

n_sims <- 999
null_preds_list <- vector("list", n_sims)

for (i in 1:n_sims) {
  
  df_null <- df_sub %>%
    group_by(Functional.group) %>%
    mutate(Altitude_scaled_null = sample(Altitude_scaled)) %>%
    ungroup()
  
  # IMPORTANT: SAME MODEL STRUCTURE AS REAL MODEL
  null_model <- gam(
    Count ~ Functional.group +
      s(Altitude_scaled_null, Functional.group, bs = "fs", k = 20) +
      s(Locality, bs = "re") +
      Year,
    data = df_null,
    family = nb(link = "log"),
    method = "REML"
  )
  
  null_newdata <- expand.grid(
    Altitude_scaled_null = alt_grid,
    Functional.group = target_groups,
    Locality = df$Locality[1],
    Year = df$Year[1]
  )
  
  preds <- predict(
    null_model,
    newdata = null_newdata,
    exclude = "s(Locality)",
    type = "response"
  )
  
  null_preds_list[[i]] <- data.frame(
    Altitude_scaled = alt_grid,
    Functional.group = null_newdata$Functional.group,
    Sim = i,
    Null_Fit = preds
  )
}

# ---------------------------------------------------------
# STEP 3: SUMMARIZE NULL MODEL
# ---------------------------------------------------------
cat("Step 3: Summarizing null model...\n")

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

# ---------------------------------------------------------
# STEP 4: FINAL PLOT (PUBLICATION READY)
# ---------------------------------------------------------
cat("Step 4: Running formal statistical test vs null...\n")

# Function to compute deviation statistic
calc_deviation <- function(obs, null) {
  sum((obs - null)^2, na.rm = TRUE)
}

results_list <- list()

for (g in target_groups) {
  
  # Observed curve
  obs_curve <- plot_data %>%
    filter(Functional.group == g) %>%
    arrange(Altitude_scaled)
  
  obs_dev <- calc_deviation(obs_curve$Obs_Fit, obs_curve$Null_Mean)
  
  # Null deviations (each simulation vs mean null)
  null_devs <- bind_rows(null_preds_list) %>%
    filter(Functional.group == g) %>%
    group_by(Sim) %>%
    arrange(Altitude_scaled) %>%
    summarize(
      dev = calc_deviation(Null_Fit, obs_curve$Null_Mean),
      .groups = "drop"
    )
  
  # p-value = proportion of null >= observed
  p_val <- mean(null_devs$dev >= obs_dev)
  
  results_list[[g]] <- data.frame(
    Functional.group = g,
    Observed_deviation = obs_dev,
    Null_mean_deviation = mean(null_devs$dev),
    p_value = p_val
  )
}

test_results <- bind_rows(results_list)

print(test_results)

cat("Step 4: Plotting...\n")

ggplot(plot_data, aes(x = Altitude_scaled)) +
  
  # NULL MODEL
  geom_ribbon(aes(ymin = Null_LCI, ymax = Null_UCI,
                  fill = "Null Expectation (95% CI)"),
              alpha = 0.5) +
  geom_line(aes(y = Null_Mean),
            color = "grey30", linetype = "dashed", size = 0.8) +
  
  # OBSERVED DATA
  geom_ribbon(aes(ymin = Obs_LCI, ymax = Obs_UCI,
                  fill = "Observed GAM (95% CI)"),
              alpha = 0.3) +
  geom_line(aes(y = Obs_Fit, color = Functional.group),
            size = 1.2) +
  
  facet_wrap(~ Functional.group, scales = "free_y", nrow = 1) +
  
  theme_bw(base_size = 14) +
  
  scale_fill_manual(values = c(
    "Null Expectation (95% CI)" = "grey60",
    "Observed GAM (95% CI)" = "black"
  )) +
  
  scale_color_manual(values = c(
    "Detritivore" = "#7570b3",
    "Omnivore" = "#d95f02",
    "Herbivore" = "#1b9e77",
    "Saproxylic" = "#00A9FF"
  )) +
  labs(
    x = "Elevational gradient (scaled)",
    y = "Predicted number of individuals",
    fill = "Uncertainty",
    color = "Functional group"
  ) +
  
  theme(
    legend.position = "right",
    strip.text = element_text(face = "bold", size = 14),
    panel.grid.minor = element_blank()
  )

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