# Pest-Analysis
# Invasive Pest Surveillance Analysis

## Project Overview
R scripts for analyzing surveillance data of three invasive pests in Canada:
- **Emerald Ash Borer** (Agrilus planipennis)
- **Asian Longhorned Beetle** (Agrilus glabripennis)
- **Hemlock Woolly Adelgid** (Adelges tsugae)

We use publicly‑available survey data from the Open Government Portal to map regulated areas, summarize detection counts and rates over time and by region, and explore potential spread beyond regulated zones.



## Data Source
- **Open Government Portal**: National pest surveillance datasets for EAB, ALB, and HWA.

## Requirements
- **R** (≥ 4.0.0)

### R Packages
```r
install.packages(c(
  "tidyverse",
  "sf",
  "tmap",
  "readxl",
  "stringr",
  "rmapshaper",
  "scales",
  "patchwork",
  "ggtext"
))
```
## Example Output
![image](https://github.com/user-attachments/assets/c481b27c-0e59-4a68-86be-ab59766aef07)
![hp_provinces](https://github.com/user-attachments/assets/f84f625a-1c32-45c5-8f7d-74c93b5415c1)



