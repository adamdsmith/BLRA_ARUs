if (!requireNamespace("nrsmisc", quietly = TRUE))
  devtools::install_github("adamdsmith/nrsmisc")
pacman::p_load(nrsmisc, dplyr)
source("R/aru_forecast.R")
source("R/aru_sequences.R")

# Cedar Island
aru_forecast(34.9417, -76.34)
seqs <- aru_sequences(34.9417, -76.34, start = "2019-04-15")
View(seqs)

# Swanquarter
aru_forecast(35.43, -76.376)

# St. Marks
aru_forecast(30.08, -84.159)

# St. Johns
aru_forecast(28.5606, -80.8832)
