# Data and Code Repository: Elevational Gradient Analysis of Ground-dwelling Arthropod Assemblages

## Overview
This repository contains the datasets and analysis scripts used to investigate changes in overall arthropod assemblage composition, beta-diversity, and functional traits along an elevational gradient. 

**Note for Peer Review:** This repository is currently in anonymous mode for double-blind peer review. Author names, affiliations, and identifying project information have been removed.

---

## Repository Structure

### 1. Data Files
The data used for the analyses are provided in `.xlsx` format.

* **`CANOCO_FINAL.xlsx`**
    * Used primarily for multivariate ordination (Section 2.3.1) and beta-diversity calculations.
    * **Sheet `sp`**: Contains the community matrix (species abundance data).
    * **Sheet `env`**: Contains corresponding environmental metadata. Variables include: `ID`, `Locality`, `Year`, `Time.period` (days since Jan 1, 2007), `Temperature`, `Precipitation`, `Site.protection`, `Altitude`, `Trees` (Dominant species: SM/Spruce or BK/Beech), `Spruce`, `Beech`, `C/N`, `pH/KCl`, `Trees(%)`, `Date`, and `Exposition`.
* **`R_FINAL.xlsx`**
    * Contains trait data for functional diversity analysis (Section 2.3.4). 
    * Includes sheets structured by taxonomic group (e.g., `spiders_FD`), containing both environmental predictors and scaled functional traits (Body Size, Trophic Strategy, Dispersal Ability, and Rao's Q).

### 2. Analytical Workflows & Scripts

#### Section 2.3.1: Ordination of overall species composition and richness
* **Software Used:** CANOCO 5 (Ter Braak & Šmilauer, 2012)
* **Methodology:** Partial Detrended Canonical Correspondence Analysis (pDCCA) was used to test overall assemblage composition. Species abundances were log-transformed (ln(x+1)) and rare species were down-weighted. Elevation was modelled as a quadratic polynomial, with sampling sites, time period, temperature, and precipitation as covariates. Edaphic factors (e.g., pH/KCl, C/N) and tree proportions were projected passively.

#### Section 2.3.2: Calculation of overall beta-diversity individual components
* **Script:** `Beta-diversity.R`
* **Methodology:** Aggregates community data by site/date and converts it to presence-absence matrices. Calculates three complementary, non-additive dissimilarity indices (Šizling et al., 2026):
    1.  **Jaccard dissimilarity:** Overall compositional dissimilarity.
    2.  **Simpson dissimilarity:** Species turnover (independent of richness).
    3.  **Richness difference index:** Species richness uniformity.
* **Statistical Tests:** Evaluates multivariate homogeneity of group dispersions via `betadisper` and tests for significance using PERMANOVA (`adonis2` in the `vegan` package).

#### Section 2.3.4: Calculation of trait-based and functional diversity indices
* **Scripts:** `GAM.R`, `Scaling and correlation.R`
* **Methodology:** * **Collinearity Checks (`Scaling and correlation.R`):** Assesses redundancy among Community-Weighted Means (CWMs) using Spearman rank correlation thresholds (|ρ| ≥ 0.75) and Principal Component Analysis (PCA) via the `FactoMineR` package.
    * **Generalized Additive Models (`GAM.R`):** Fits GAMs (`mgcv` package) to evaluate functional indices along the elevational gradient. Elevation and Exposition are standardized. Time period and Locality are treated as random effects (`bs = "re"`).
        * **Body Size:** Modelled with Gaussian family (log link).
        * **Trophic Strategy & Dispersal:** Scaled to the interval (0, 1) and modelled using Beta regression (`betar`).
        * **Rao's Quadratic Entropy (Rao's Q):** Modelled using the Tweedie distribution (`tw`, log link).

#### Spatial Autocorrelation Testing
* **Script:** `Variogram.R`
* **Methodology:** Tests for residual spatial autocorrelation to ensure model robustness. Converts WGS84 coordinates to UTM zone 33N (`sf` package). Calculates global Moran's I (`spdep` package) using row-standardized spatial weights matrices (k=1 nearest neighbor) and constructs empirical semi-variograms (`gstat` package) of site-averaged Pearson residuals.

#### Visualizations
The following scripts generate the graphical outputs (contour biplots, GAM smooths, and partial effects) used in the manuscript figures and supplementary materials:
* `Body_size_GAM_visualization.R`
* `Dispersal_ability_GAM_visualization.R`
* `Trophic_strategy_GAM_visualization.R`
* `FD_Rao_GAM_visualization.R`

---

## Dependencies

Analyses were conducted in R. To run the scripts, ensure the following packages are installed:
* **Data Manipulation & General:** `dplyr`, `tidyr`, `readxl`, `stringr`
* **Community Ecology:** `vegan`
* **Modelling:** `mgcv`, `gratia` (for GAM visualization/evaluation)
* **Spatial Analysis:** `sf`, `spdep`, `gstat`
* **Multivariate Statistics:** `FactoMineR`, `corrplot`
* **Data Visualization:** `ggplot2`
