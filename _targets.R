# _targets.R file
library(targets)

# source files
source("R/variables.R")
source("R/bechdel.R")
source("R/plots.R")

# targets
tar_option_set(packages = c("readr", "dplyr", "ggplot2", "tidyr", "purrr", "tibble", "stringr"))
list(
  tar_target(file, "data/script.csv", format = "file"),
  tar_target(raw_data, get_data(file)),
  tar_target(tests, bechdel_test(raw_data, female_names, male_names)),
  tar_target(overall_plot_output, overall_plot(tests)),
  tar_target(individual_plot_output, individual_plot(tests))
)