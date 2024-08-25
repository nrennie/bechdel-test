library(tidyverse)

# Read in data
dialogue <- readr::read_csv("https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv")


dialogue |> 
  drop_na(dialogue, stage_direction) |> 
  select(stage_direction) |> 
  distinct() |> 
  filter(str_detect(stage_direction, '[:upper:]')) |> 
  View()


# List of characters
male_names <- c(
  "Mike", "Jim", "Dustin", "Lucas", "Will", "Ted", "Troy",
  "Ben", "Mr. Clarke", "Hopper", "Jonathan", "Callahan", "Lonnie",
  "Powell", "Steve", "Tommy", "Donald", "Earl", "Ronald Reagan",
  "O'Bannon", "Shepard", "Pastor Charles", "Dr. Brenner", "Reed",
  "Axel", "Mick", "Keith", "Murray", "Merrill", "Owens", "Bob",
  "Jack", "Billy", "Teddy", "Mr. Sinclair", "Terry", "Ray",
  "Funshine", "Neil", "Sam", "Bruce", "Tom", "Larry", "Grigori",
  "Alexei", "Todd", "Marty", "Doc", "Ten", "Argyle", "Eddie",
  "Fred", "Enzo", "Wayne", "Sullivan", "Daniels", "Gareth", "Jason",
  "Patrick", "Antonov", "Hatch", "Harmon", "Victor", "Brenner",
  "Andy", "Wallace", "Cornelius", "Yuri", "Tanner", "Tatum", "Two",
  "One", "Louis Armstrong", "Armstrong"
)

female_names <- c(
  "Eleven", "Nancy", "Joyce", "Barb", "Max", "Liz", "Karen",
  "Barbara", "Florence", "Carol", "Holly", "Cynthia", "Sandra",
  "Ms. Holland", "Nicole", "Becky", "Connie", "Sarah", "Dottie",
  "Tina", "Marsha", "Mrs. Sinclair", "Erica", "Claudia", "Lynn",
  "Kali", "Susan", "Diane", "Mrs. Driscoll", "Robin", "Flo",
  "Heather", "Candice", "Erica", "Suzie", "Mrs. Ergenbright",
  "Angela", "Mrs. Gracey", "Chrissy", "Kelley", "Alice", "Eden",
  "Mrs. Henderson", "Mrs. Wheeler", "Virginia", "Vickie", "Fitzgerald"
)

s1e1 <- dialogue |>
  filter(season == 1, episode == 1) |>
  select(line, stage_direction, dialogue)

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

test1(s1e1, female_names)


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

test2(s1e1, female_names)


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




# TODO
# finish test 3 and to function
# wrapper function
# package
# vignette
# edit female_lines code (contains)




# generalise to function
# output vector that starts all false
# run across all episodes
# visualise (heatmap)

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
    result[3] <- test3(data = data, female_names = female_names, male_names = male_names)
  }
  return(result)
}

bechdel_test_function(data, female_names, male_names)


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
unique_episodes <- dialogue |>
  select(season, episode) |>
  distinct() |>
  arrange(season, episode)

# Apply to all episodes
all_tests <- unique_episodes %>%
  purrr::map2(
    .x = pull(., season),
    .y = pull(., episode),
    .f = ~ test_episode(
      s = .x,
      e = .x,
      data = dialogue,
      male_names = male_names,
      female_names = female_names
    )
  )  |> 
  unlist() |> 
  matrix(ncol = 3, byrow = TRUE) |>
  as_tibble(.name_repair = "unique") |>
  rename("test1" = 1, "test2" = 2, "test3" = 3) |>
  mutate(overall = all(test1, test2, test3)) |>
  cbind(unique_episodes) |>
  as_tibble()

# save as CSV
write_csv(all_tests, "all_tests_R.csv")
