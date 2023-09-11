library(tidyverse)

# Read in data
episodes <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/episodes.csv')
dialogue <- readr::read_csv('https://raw.githubusercontent.com/rfordatascience/tidytuesday/master/data/2022/2022-10-18/stranger_things_all_dialogue.csv')

# male characters
# female characters
# finish lists of characters

male <- c("Mike", "Jim")
female <- c("Eleven", "Nancy", "Joyce", "Barb", "Max")
female_lines <- paste0("[", female, "]")

# First episode only
s1e1 <- dialogue |> filter(season == 1, episode == 1)

# Test 1: At least two women in it
# At least two unique values
s1e1_female <- s1e1 |> 
  filter(stage_direction %in% female_lines)
if (length(unique(s1e1_female$stage_direction)) >= 2) {
  print("Test 1 Passed!")
} else {
  print("Test 1 Failed!")
}

# Test 2: Women talk to each other
# Two consecutive values (line)
s1e1_female_conv <- s1e1_female |> 
  mutate(lag_line = lag(line) + 1, .after = line) |> 
  filter(line == lag_line)
if (nrow(s1e1_female_conv) > 0) {
  print("Test 2 Passed!")
} else {
  print("Test 2 Failed!")
} 

#TODO this leaves a missing line (should check before or after)

# Test 3: about something besides a man 
s1e1_about_men <- s1e1_female_conv |> 
  select(dialogue) |> 
  mutate(about_man = str_detect(dialogue, "man")) ##### more terms here
if (!any(s1e1_about_men$about_man)) {
  print("Test 3 Passed!")
} else {
  print("Test 3 Failed!")
} 



# generalise to function
# output vector that starts all false
# run across all episodes
# visualise (heatmap)

bechdel_test_function <- function(
    data, s, e, female, male
) {
  
  episode_data <- data |> 
    filter(season == s, episode == e)
  
  # prep output
  test_result <- rep(FALSE, 3)
  
  # female lines stage direction
  female_lines <- paste0("[", female, "]")
  
  # test 1 : At least two women in it
  s1e1_female <- episode_data |> 
    filter(stage_direction %in% female_lines)
  test1 <- length(unique(s1e1_female$stage_direction)) >= 2
  
  if (test1) {
    test_result[1] <- TRUE
    
    # test 2 : Women talk to each other
    s1e1_female_conv <- s1e1_female |> 
      mutate(lag_line = lag(line) + 1, .after = line) |> 
      filter(line == lag_line)
    test2 <- nrow(s1e1_female_conv) > 0
    
    if (test2) {
      test_result[2] <- TRUE
      
      # test 3 : about something besides a man 
      s1e1_about_men <- s1e1_female_conv |> 
        select(dialogue) |> 
        mutate(about_man = str_detect(dialogue, "man")) # edit
      test3 <- !any(s1e1_about_men$about_man)
      
      if (test3) {
        test_result[3] <- TRUE
      }
      
    }
  }
  # output
  return(test_result)
}

male <- c("Mike", "Jim")
female <- c("Eleven", "Nancy", "Joyce", "Barb", "Max")
bechdel_test_function(dialogue,
                      s = 1,
                      e = 1,
                      male = male,
                      female = female)



# Test each episode -------------------------------------------------------

# list of unique episodes
unique_episodes <- dialogue %>%
  select(season, episode) %>%
  distinct() 

# apply to all episodes
all_tests <- unique_episodes %>% 
  purrr::map2(.x = pull(., season),
              .y = pull(., episode),
              .f = ~bechdel_test_function(dialogue,
                                          s = .x,
                                          e = .x,
                                          male = male,
                                          female = female)) |> 
  unlist() %>%
  matrix(ncol = 3, byrow = TRUE) |> 
  as_tibble(.name_repair = "unique") |> 
  rename("test1" = 1, "test2" = 2, "test3" = 3) |> 
  mutate(overall = all(test1, test2, test3)) |> 
  cbind(unique_episodes) |> 
  as_tibble()

# save as CSV
write_csv(all_tests, "all_tests_R.csv")
