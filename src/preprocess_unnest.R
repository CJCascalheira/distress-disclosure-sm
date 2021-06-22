# Dependencies
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(SnowballC)

# Import data
trans_sports_full <- readRDS("data/trans_sports_full.rds")

# Load stop words
data(stop_words)

# Custom stop words
custom_stop_words <- tribble(
  ~word, ~lexicon,
  "https", "CUSTOM",
  "t.co", "CUSTOM",
  "tco", "CUSTOM",
  "amp", "CUSTOM",
  "rt", "CUSTOM", 
  "en", "CUSTOM",
)
custom_stop_words

# Language in this dataset?
table(trans_sports_full$lang)

# Select relevant columns
trans_sports_full1 <- trans_sports_full %>%
  mutate(text_original = text) %>%
  select(author_id, created_at, text, everything()) %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  # Remove Korean words
  filter(author_id != trans_sports_full[22488, ]$author_id) %>%
  # Remove white space and URLS
  mutate(
    text = str_replace_all(text, "\n", " "),
    text = str_replace_all(text, "(^|[:space:])(www|http)(.*?)($|[:space:])", " "),
    text = str_replace_all(text, "[:space:]{2,}", " ")
  ) %>%
  # Remove punctuation
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[0-9]+", " ")) %>%
  # Remove odd characters
  mutate(text = str_replace_all(text, "[\\W+]", " ")) %>%
  unnest_tokens(word, text) %>%
  # Remove stop words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  # Reduce to word stem
  mutate(word = wordStem(word, language = "en")) %>%
  # Move main variables to the front
  select(author_id, created_at, word, everything())
trans_sports_full1

# Save to file
saveRDS(trans_sports_full1, file = "data/trans_sports_clean.rds")
