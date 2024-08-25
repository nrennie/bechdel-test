# Read in data
get_data <- function(file) {
  read_csv(file, col_types = cols())
}

# Test 1: At least two women in it
test1 <- function(data, female_names) {
  female_lines_query <- paste0(female_names, collapse = "|")
  data_female <- data |>
    filter(str_detect(character, female_lines_query))
  if (length(unique(data_female$character)) >= 2) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test 2: Women talk to each other
test2 <- function(data, female_names) {
  female_lines_query <- paste0(female_names, collapse = "|")
  data_female <- data |>
    mutate(
      para_number = consecutive_id(character)
    ) |>
    filter(str_detect(character, female_lines_query)) |>
    mutate(
      line_diff = c(NA, diff(line_number)),
      para_diff = c(NA, diff(para_number))
    )
  female_conv <- data_female |>
    filter(line_diff == 1, para_diff != 0)
  if (nrow(female_conv) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Test 3: about something besides a man
test3 <- function(data, female_names, male_names) {
  female_lines_query <- paste0(female_names, collapse = "|")
  data_female <- data |>
    mutate(
      para_number = consecutive_id(character)
    ) |>
    filter(str_detect(character, female_lines_query)) |>
    mutate(
      line_diff = c(NA, diff(line_number)),
      para_diff = c(NA, diff(para_number))
    )
  female_conv <- data_female |>
    filter(line_diff == 1, para_diff != 0)

  # filter dialogue containing any of the following terms
  male_terms <- c(male_names, " he ", " him ", " his ", " he's ")
  male_terms_query <- paste0(male_terms, collapse = "|")
  not_about_men <- female_conv |>
    filter(str_detect(dialogue, male_terms_query, negate = TRUE))

  if (nrow(not_about_men) > 0) {
    return(TRUE)
  } else {
    return(FALSE)
  }
}

# Run all three tests
bechdel_test_function <- function(data, female_names, male_names) {
  result <- rep(FALSE, 3)
  # Test 1
  result[1] <- test1(
    data = data,
    female_names = female_names
  )
  # Test 2
  if (result[1]) {
    result[2] <- test2(
      data = data,
      female_names = female_names
    )
  }
  # Test 3
  if (result[2]) {
    result[3] <- test3(
      data = data,
      female_names = female_names,
      male_names = male_names
    )
  }
  return(result)
}


# Test each scene -------------------------------------------------------

# Function to apply to act and scene data
test_scene <- function(act, scene, data, female_names, male_names) {
  scene_data <- data |>
    filter(act == act, scene == scene)
  bechdel_test_function(
    data = scene_data,
    female_names = female_names,
    male_names = male_names
  )
}

# List of unique scenes
bechdel_test <- function(
    data,
    female_names,
    male_names) {
  unique_scenes <- data |>
    select(act, scene) |>
    distinct()

  # Apply to all scenes
  suppressMessages(all_tests <- unique_scenes %>%
    map2(
      .x = pull(., act),
      .y = pull(., scene),
      .f = ~ test_scene(
        act = .x,
        scene = .y,
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
    cbind(unique_scenes) |>
    as_tibble())
  return(all_tests)
}
