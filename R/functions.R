# Read in data
get_data <- function(file) {
  read_csv(file, col_types = cols())
}

# Test 1: At least two women in it
test1 <- function(data, female_names) {
  female_lines <- paste0("[", female_names, "]")
  data_female <- data |>
    filter(stage_direction %in% female_lines)
  if (length(unique(data_female$stage_direction)) >= 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test 2: Women talk to each other
test2 <- function(data, female_names) {
  female_lines <- paste0("[", female_names, "]")
  data_female <- data |>
    filter(stage_direction %in% female_lines)
  if (any(diff(data_female$line) == 1)) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test 3: about something besides a man
test3 <- function(data, female_names, male_names) {
  female_lines <- paste0("[", female_names, "]")
  data_female <- data |>
    filter(stage_direction %in% female_lines)
  data_female$diff <- c(NA, diff(data_female$line))
  # filter diff == 1

  # filter dialogue containing any of the following terms
  male_terms <- c(male_names, "he", "him", "his", "he's")


  if (nrow(data_female) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Run all three tests
bechdel_test_function <- function(data, female_names, male_names) {
  result <- rep(FALSE, 3)
  # Test 1
  result[1] <- test1(data = data, female_names = female_names)
  # Test 2
  if (result[1]) {
    result[2] <- test2(data = data, female_names = female_names)
  }
  # Test 3
  if (result[2]) {
    result[3] <- test3(
      data = data,
      female_names = female_names, male_names = male_names
    )
  }
  return(result)
}



# Test each episode -------------------------------------------------------

# Function to apply to season and episode data
test_episode <- function(season, episode, data, female_names, male_names) {
  episode_data <- data |>
    filter(season == season, episode == episode) |>
    select(line, stage_direction, dialogue)
  bechdel_test_function(
    data = episode_data,
    female_names = female_names,
    male_names = male_names
  )
}

# List of unique episodes
test_all_episodes <- function(
    data,
    female_names,
    male_names) {
  unique_episodes <- data |>
    select(season, episode) |>
    distinct() |>
    arrange(season, episode)

  # Apply to all episodes
  suppressMessages(all_tests <- unique_episodes %>%
    map2(
      .x = pull(., season),
      .y = pull(., episode),
      .f = ~ test_episode(
        s = .x,
        e = .y,
        data = data,
        male_names = male_names,
        female_names = female_names
      )
    ) |>
    unlist() |>
    matrix(ncol = 3, byrow = TRUE) |>
    as_tibble(.name_repair = "unique") |>
    rename("test1" = 1, "test2" = 2, "test3" = 3) |>
    mutate(overall = all(test1, test2, test3)) |>
    cbind(unique_episodes) |>
    as_tibble())
  return(all_tests)
}
