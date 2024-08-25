# Heat map of act and scene all pass
overall_plot <- function(all_tests) {
  g <- all_tests |>
    select(act, scene, overall) |>
    mutate(overall = factor(overall, levels = c("TRUE", "FALSE"))) |>
    ggplot() +
    geom_tile(
      mapping = aes(x = factor(scene), y = factor(act), fill = overall),
      colour = "black"
    ) +
    scale_fill_manual(
      values = c("TRUE" = "grey20", "FALSE" = "grey90"),
      labels = c("TRUE" = "Pass", "FALSE" = "Fail"),
      drop = FALSE
    ) +
    labs(x = NULL, y = NULL, title = "Does Romeo and Juliet pass the Bechdel test?") +
    scale_y_discrete(limits = rev) +
    coord_cartesian(expand = FALSE) +
    theme_minimal() +
    theme(
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid = element_blank()
    )
  ggsave("results/overall_plot.png", width = 8, height = 4, bg = "white")
}

# Heat map of three tests and scene and act along bottom
individual_plot <- function(all_tests) {
  all_tests |>
    mutate(
      scene_label = paste0(act, ", ", scene)
    ) |>
    select(scene_label, test1, test2, test3) |>
    pivot_longer(
      -scene_label,
      names_to = "test", values_to = "value"
    ) |>
    mutate(value = factor(value, levels = c("TRUE", "FALSE"))) |>
    ggplot() +
    geom_tile(
      mapping = aes(x = scene_label, y = test, fill = value),
      colour = "black"
    ) +
    scale_fill_manual(
      values = c("TRUE" = "grey20", "FALSE" = "grey90"),
      drop = FALSE
    ) +
    scale_y_discrete(
      labels = c(
        "test1" = "There are at least two named women",
        "test2" = "who speak to each other",
        "test3" = "about something other than a man"
      ),
      limits = rev
    ) +
    labs(x = NULL, y = NULL) +
    coord_cartesian(expand = FALSE) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 90),
      legend.position = "top",
      legend.title = element_blank(),
      panel.grid = element_blank()
    )
  ggsave("results/individual_plot.png", width = 8, height = 4, bg = "white")
}
