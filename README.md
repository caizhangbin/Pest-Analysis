# Pest-Analysis
# Invasive Pest Surveillance Analysis

## Project Overview
This repository contains the written exam report and accompanying R scripts for analyzing surveillance data of three invasive pests in Canada:
- **Emerald Ash Borer** (Agrilus planipennis)
- **Asian Longhorned Beetle** (Agrilus glabripennis)
- **Hemlock Woolly Adelgid** (Adelges tsugae)

We use publicly‑available survey data from the Open Government Portal to map regulated areas, summarize detection counts and rates over time and by region, and explore potential spread beyond regulated zones.


## Data Source
- **Open Government Portal**: National pest surveillance datasets for EAB, ALB, and HWA.

## Requirements
- **R** (≥ 4.0.0)
- R packages:
  - `tidyverse`
  - `sf`
  - `ggplot2`
  - `ggthemes`
  - `cowplot`
  - `lubridate`

You can install all dependencies with:
```r
install.packages(c("tidyverse", "sf", "ggplot2", "ggthemes", "cowplot", "lubridate"))
