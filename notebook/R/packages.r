# include the following packages
packages <- c(
  "assertthat",
  "broom",
  "extrafont",
  "ggrepel",
  "ggpubr",
  "gridExtra",
  "Hmisc",
  "lme4",
  "lubridate",
  "kableExtra",
  "knitr",
  "magrittr",
  "plyr",
  "readr",
  "rmarkdown",
  "rio",
  "sjmisc",
  "sjPlot",
  "tidyverse",
  "devtools"
)

if (!require(pacman)) install.packages("pacman")
pacman::p_load(char = packages)
# devtools::install_github("strengejacke/ggeffects")
