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

# Load sentiment library
afinn <- get_sentiments(lexicon = "afinn")
afinn

# Language in this dataset?
table(trans_sports_full$lang)

# Select relevant columns
trans_sports_full1 <- trans_sports_full %>%
  mutate(created_at = ymd_hms(created_at)) %>%
  # Remove Korean words
  filter(author_id != trans_sports_full[22488, ]$author_id) %>%
  unnest_tokens(word, text) %>%
  select(author_id, created_at, word, everything()) %>%
  # Remove stop words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  # Reduce to word stem
  mutate(word = wordStem(word, language = "en")) %>%
  mutate(word = str_replace_all(word, "[[:punct:]]", "")) %>%
  mutate(word = str_replace_all(word, "[0-9]+", ""))
trans_sports_full1
