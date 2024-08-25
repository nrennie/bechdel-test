library(rvest)
library(tidyverse)

romeo_juliet_raw <- rvest::read_html("https://shakespeare.mit.edu/romeo_juliet/full.html")

str_extract_between <- function(x, start, end) {
  pattern <- paste0("(?<=", start, ")(.*?)(?=", end, ")")
  return(stringr::str_extract(x, pattern = pattern))
}

a_char <- romeo_juliet_raw |> 
  html_elements("a") |> 
  as.character()

script <- tibble(raw_char = a_char) |> 
  # Remove top two title rows
  slice(-c(1, 2)) |> 
  # Who is speaking?
  mutate(
    character = case_when(
      str_detect(raw_char, "<b>") ~ str_extract_between(raw_char, "<b>", "</b>"),
      TRUE ~ NA_character_
    ),
    character = str_to_title(character)
  ) |> 
  mutate(
    character_line = str_detect(raw_char, "<b>")
  ) |> 
  fill(character, .direction = "down") |> 
  filter(!character_line) |> 
  select(-character_line) |> 
  # What are they saying?
  mutate(
    dialogue = str_extract_between(
      raw_char, '>', '</a>'
    )
  ) |> 
  select(-raw_char) |> 
  mutate(
    character = replace_na(character, "Chorus")
  ) |> 
  mutate(line_number = row_number()) |> 
  mutate(
    character = case_when(
      character == "Lady  Capulet" ~ "Lady Capulet",
      TRUE ~ character
    )
  )

write_csv(script, "data/script.csv")
