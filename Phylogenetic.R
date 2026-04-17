library(dplyr)
library(gstat)
library(sp)
library(spdep)
library(mgcv)
library(readxl)
library(writexl)
library(picante)
library(ape)
# Calculation of PD, MPD, and SESpd #
commun <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_compo_names")
tree_data <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "Phylo_chilo")
tree_data[] <- lapply(tree_data, factor)
commun_df <- as.data.frame(commun)

# Order/Family/Family_name/Species
# Create the tree using the EXACT column names from your Excel sheet
tree.p <- as.phylo(~Subfamily/Family_name/species, data=tree_data)
treeRoot <- multi2di(tree.p)
tree.pp <- compute.brlen(treeRoot)

# Use picante's built-in tool to prune the tree and community matrix 
combined <- match.phylo.comm(tree.pp, commun_df)
tree_matched <- combined$phy
comm_matched <- combined$comm

# Calculate ses.pd (Standardized Effect Size of Faith's PD)
my_ses_pd <- ses.pd(comm_matched, tree_matched, null.model="independentswap", runs=500)
# pd.obs.z
write_xlsx(my_ses_pd, "SESpd.xlsx")

# Calculate Mean Pairwise Distance (MPD) and ses.mpd #
phydist <- cophenetic(tree_matched)
my_raw_mpd <- mpd(comm_matched, phydist, abundance.weighted=TRUE)
my_ses_mpd <- ses.mpd(comm_matched, phydist, null.model="independentswap", abundance.weighted=TRUE, runs=500)
# MPD = mpd.obs
write_xlsx(my_ses_mpd, "MPD.xlsx")

# Analysis of Phylogeny Sespd and meanPD #
PD <- read_excel("Beskydy_2007_2008_traits_final.xlsx", sheet = "chilo_diplo_iso_FD")
PD$Locality <- as.factor(PD$Locality)
PD$Year     <- as.factor(PD$Year)
PD$Altitude_scaled <- as.numeric(scale(PD$Altitude, center = TRUE, scale = TRUE))
PD <- PD %>%
  mutate(
    Exposition2 = sapply(strsplit(as.character(Exposition), "_"), function(x) mean(as.numeric(x))),
    Exposition2 = as.numeric(scale(Exposition2))
  )

mod_gam_pd <- gam(
  MPD ~ s(Locality, bs = "re") +
    Altitude_scaled + Exposition2 + Site.protection + Year,
  data = PD, 
  family = gaussian(), 
  method = "REML"
)

summary(mod_gam_pd)
par(mfrow = c(2, 2))
gam.check(mod_gam_pd)
concurvity(mod_gam_pd, full = TRUE)
gratia::draw(mod_gam_pd)
plot(mod_gam_pd, select = 2)

# correlogram (autocorrelation using Moran’s I based on site-averaged Pearson residuals)
library(DHARMa)
library(qgam)
library(mgcViz)
library(dplyr)
library(gstat)
library(sp)
library(spdep)

PD$resid <- residuals(mod_gam_pd, type = "pearson")
df_site_res <- PD %>%
  group_by(Locality, X_km, Y_km) %>%
  summarise(mean_res = mean(resid, na.rm = TRUE), .groups = "drop")
coords <- as.matrix(df_site_res[,c("X_km","Y_km")])
nb <- dnearneigh(coords, 0, 10)   
lw <- nb2listw(nb, style = "W")
moran.test(df_site_res$mean_res, lw)
coordinates(df_site_res) <- ~X_km + Y_km
vg <- variogram(mean_res ~ 1,
                data = df_site_res,
                cutoff = 40,
                width = 2,
                cressie = TRUE)
plot(vg, main = "Empirical variogram of GAM residuals")

# Because multiple observations were collected within the same sites (hierarchical structure), locality was included as a random effect to account for spatial clustering and avoid pseudoreplication.
PD %>%
  group_by(Locality) %>%
  summarise(alt = mean(Altitude_scaled))

### Graphical vizualization of SES.pd ###
mod_gam_sespd <- gam(
  SESpd ~ s(Locality, bs = "re") +
    s(Altitude_scaled, bs = "cr",k=3) + Exposition2 + Site.protection + Year,
  data = PD, 
  family = gaussian(), 
  method = "REML"
)

library(ggplot2)
newdat <- data.frame(
  Altitude_scaled = seq(min(PD$Altitude_scaled, na.rm = TRUE), max(PD$Altitude_scaled, na.rm = TRUE), length = 200),
  Exposition2 = mean(PD$Exposition2, na.rm = TRUE),
  Locality = PD$Locality[1], 
  Year = PD$Year[1],
  Site.protection = PD$Site.protection[1] 
)
pred <- predict(mod_gam_sespd, newdata = newdat, se.fit = TRUE,
                exclude = c("s(Locality)", "s(Year)"))
newdat$fit <- pred$fit
newdat$se  <- pred$se.fit
newdat$upper <- newdat$fit + 1.96 * newdat$se
newdat$lower <- newdat$fit - 1.96 * newdat$se

phylo<-ggplot(PD, aes(x = Altitude_scaled, y = SESpd)) +
  # The raw points - now matching the CWM plot using geom_jitter
  geom_jitter(width = 0.03, height = 0, size = 1.8, alpha = 0.6, color = "black") +  
  
  # The GAM trendline
  geom_smooth(method = "glm", color = "black", fill = "grey70", alpha = 0.3) +
  
  # The 0 baseline 
  geom_hline(yintercept = 0, linetype = "solid", color = "black", linewidth = 0.5, alpha = 0.6) + 
  
  # The +/- 1.96 significance thresholds
  geom_hline(yintercept = 1.96, linetype = "dashed", color = "black", alpha = 0.7) +
  geom_hline(yintercept = -1.96, linetype = "dashed", color = "black", alpha = 0.7) +
  
  # X-Axis: Matches the CWM plot limits exactly
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  
  theme_minimal() +
  labs(
    x = "Elevational gradient (Scaled)",
    y = "SESpd") +
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
phylo


# Graph for MPD #
newdat_mpd <- data.frame(
  Altitude_scaled = seq(min(PD$Altitude_scaled, na.rm = TRUE), max(PD$Altitude_scaled, na.rm = TRUE), length = 200),
  Exposition2 = mean(PD$Exposition2, na.rm = TRUE),
  Locality = PD$Locality[1], 
  Year = PD$Year[1],
  Site.protection = PD$Site.protection[1] 
)

# 2. Predict using the MPD model (mod_gam_pd)
pred_mpd <- predict(mod_gam_pd, 
                    newdata = newdat_mpd, 
                    se.fit = TRUE,
                    exclude = "s(Locality)")

# 3. Calculate the confidence intervals
newdat_mpd$fit <- pred_mpd$fit
newdat_mpd$se  <- pred_mpd$se.fit
newdat_mpd$upper <- newdat_mpd$fit + 1.96 * newdat_mpd$se
newdat_mpd$lower <- newdat_mpd$fit - 1.96 * newdat_mpd$se

phylo_mpd <- ggplot(PD, aes(x = Altitude_scaled, y = MPD)) + # Make sure y is MPD here!
  
  # The raw points
  geom_jitter(width = 0.03, height = 0, size = 1.8, alpha = 0.6, color = "black") +  
  
  # Draw the confidence interval using newdat_mpd
  geom_ribbon(data = newdat_mpd, aes(y = fit, ymin = lower, ymax = upper), fill = "grey70", alpha = 0.3) +
  
  # Draw the linear GAM trendline using newdat_mpd
  geom_line(data = newdat_mpd, aes(y = fit), color = "black", linewidth = 1) +
  
  # (Notice I removed the 0 and 1.96 horizontal lines, as they don't apply to raw MPD!)
  
  # X-Axis limits
  scale_x_continuous(breaks = seq(-2, 2, 1), minor_breaks = NULL) +
  
  theme_minimal() +
  labs(
    x = "Elevational gradient (Scaled)",
    y = "Mean pairwise distance (MPD)"
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

phylo_mpd
