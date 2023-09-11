library(tidyverse)

# read in results
all_tests <- readr::read_csv("all_tests_R.csv")

# heatmap of season and episode all pass
all_tests |> 
  select(season, episode, overall) |> 
  ggplot() +
  geom_tile(aes(x = factor(episode), y = factor(season), fill = overall)) +
  theme_minimal()

# heatmap of three tests and episode and season aolng bottom
all_tests |> 
  mutate(
    episode_label = paste0("Season ", season, " Episode ", episode)
    ) |> 
  select(episode_label, test1, test2, test3) |> 
  pivot_longer(
    -episode_label, names_to = "test", values_to = "value"
    ) |> 
  ggplot() +
  geom_tile(
    aes(x = episode_label, y = test, fill = value)
    ) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 90))
