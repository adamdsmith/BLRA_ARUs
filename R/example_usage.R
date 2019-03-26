if (!requireNamespace("devtools", quietly = TRUE))
  install.packages("devtools", quiet = TRUE)
if (!requireNamespace("remotes", quietly = TRUE))
  install.packages("remotes", quiet = TRUE)
if (!requireNamespace("nrsmisc", quietly = TRUE))
  remotes::install_github("adamdsmith/nrsmisc")
if (!requireNamespace("pacman", quietly = TRUE))
  install.packages("pacman", quiet = TRUE)
pacman::p_load(nrsmisc, dplyr)
devtools::source_url("https://raw.githubusercontent.com/adamdsmith/BLRA_ARUs/master/R/aru_forecast.R")

# Cedar Island
# Get forecast for next week
aru_forecast(34.9417, -76.34)

# Swanquarter
aru_forecast(35.43, -76.376)

# Alligator River
aru_forecast(35.8854, -75.7612)

# St. Marks
aru_forecast(30.08, -84.159)

# St. Johns
aru_forecast(28.5606, -80.8832)
