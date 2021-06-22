# Dependencies
library(tidyverse)
library(tidytext)
library(SnowballC)

# Import data
trans_sports_full <- readRDS("data/trans_sports_full.rds")

# Load stop words
data(stop_words)

# Select relevant columns
trans_sports_full1 <- trans_sports_full %>%
  select(author_id, created_at, text) %>%
  mutate(created_at = ymd_hms(created_at))

# Distress dictionary
search_dictionary <- c('tired', 'fatigued', 'fatigue', 'exhausted', 'exhaustion', 'nervous',
                       'irritated', 'hopeless', 'despaired', 'desperate', 'depression',
                       'restless', 'fidget', 'on edge', 'depressed', 'sad', 'unhappy',
                       'valueless', 'useless', 'empty', 'crying', 'tearful',
                       "no appetite", "don't feel like eating",
                       "can't stop eating", "eat everything in sight",
                       "can't get anything done", "can't get motivated",
                       "guilt", "guilty", "I'm stupid", "I'm not cool", "indecisive",
                       "can't decide on something", "don't feel like deciding",
                       "can't make up my mind", 'U+1F614', 'U+1F61E',
                       'U+1F622', 'U+1F629', 'U+1F62D')

# Word stems of distress dictionary
search_dictionary <- wordStem(search_dictionary, language = "en")

# Pre-process dataset
trans_sports_tokens <- unnest_tokens(trans_sports_full1, token, text) %>%
  # Remove stop words
  anti_join(stop_words, by = c("token" = "word")) 
trans_sports_tokens

# Apply filter
trans_sports_tokens %>%
  # Just the stop words
  mutate(token = wordStem(token, language = "en")) %>%
  filter(token %in% search_dictionary)
