library(targets)

source("targets/_targets_config.R")
sapply(list.files("targets/scripts/", full.names = TRUE), source)
source("targets/_config.R")

list(
  tar_target(
    "data_track",
    config$path_to_file,
    format = "file"
  )
)