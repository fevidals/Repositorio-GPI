
#áquetes utilizados por los autores y necesarios para reproducción
# Paquetes disponibles en CRAN
cran_packages <- c(
  "blockTools",
  "DeclareDesign",
  "dplyr",
  "FNN",
  "foreign",
  "glue",
  "gt",
  "haven",
  "Hmisc",
  "kableExtra",
  "lmtest",
  "lubridate",
  "metafor",
  "nnet",
  "patchwork",
  "randomizr",
  "RANN",
  "readstata13",
  "readxl",
  "ri2",
  "RItools",
  "sf",
  "showtext",
  "sp",
  "tidyverse", 
  "fastDummies", 
  "broom", 
  "here"
)

install.packages(cran_packages, repos = "https://cran.rstudio.com/")

# stdidx viene de GitHub (no está en CRAN)
if (!require("remotes")) install.packages("remotes")
remotes::install_github("sdouglas/stdidx")

# colorout solo funciona en Mac/Linux, en Windows lo saltamos
# rgdal fue retirado de CRAN, usamos sf como reemplazo moderno
# ogrDrivers y readOGR venían de rgdal, sf los reemplaza

# Congela las versiones
renv::snapshot()