# _targets.R

library(targets)
# source("R/functions.R")

tar_option_set(packages = c("tidyverse", "googlesheets4"))

list(
  tar_target(
    raw_data_file,
    "data/raw_data.csv",
    format = "file"
  )
)
