# _targets.R file
library(targets)
source("R/functions.R")
source("R/variables.R")
source("R/plots.R")
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "tidyr", "purrr", "tibble"))
list(
  tar_target(file, "data/dialogue.csv", format = "file"),
  tar_target(raw_data, get_data(file)),
  tar_target(tests, test_all_episodes(raw_data, female_names, male_names)),
  tar_target(overall_plot_output, overall_plot(tests)),
  tar_target(individual_plot_output, individual_plot(tests))
)