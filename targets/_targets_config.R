packages <- list(
  default = c("tidyverse")
)

options(tidyvers.quiet = TRUE)

tar_option_set(
  packages = packages$default
)

tar_option_set(
  iteration = "list"
)