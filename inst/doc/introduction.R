## ----include = FALSE----------------------------------------------------------
knitr::opts_chunk$set(
  collapse = TRUE,
  comment = "#>"
)

## ----eval = F-----------------------------------------------------------------
# # Install from CRAN (if available)
#  install.packages("translit.uk")
# 
# # Or install from GitHub
# # install.packages("devtools")
# devtools::install_github("amice13/translit.uk", force = T)

## ----eval = F-----------------------------------------------------------------
# library(translit.uk)
# translit('строка')

## ----eval = F-----------------------------------------------------------------
# library(translit.uk)
# 
# # Transliterate a simple Ukrainian word
# translit("згадка")
# 
# # Output: "zhadka"

