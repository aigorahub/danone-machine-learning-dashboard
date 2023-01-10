
# renv::install("aigorahub/aigoraOpen")
# renv::install("aigorahub/aigoraML")

default_packages <- c("tidyverse")
modeling_packages <- c("tidyverse", "tidymodels", "aigoraOpen")
modelstudio_packages <- c(modeling_packages, "DALEX", "modelStudio")
reporting_packages <- c("tidyverse", "officer")
optimization_packages <- c("tidyverse", "tidymodels", "aigoraOpen", "reticulate")

tar_option_set(
  packages = default_packages
)

options(tidyvers.quiet = TRUE)

# tar_option_set(workspaces = "model_fit_nnet_sensory_t_overall_liking_liking_product")
tar_option_set(
  iteration = "list",
  error = "continue"
  )