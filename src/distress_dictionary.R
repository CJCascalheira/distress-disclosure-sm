# Dependencies
library(tidyverse)
library(tidytext)
library(SnowballC)
library(lubridate)

# Import data
trans_sports_full <- readRDS("data/trans_sports_clean_words.rds")

# Load stop words
data(stop_words)

# Select relevant columns
trans_sports_full1 <- trans_sports_full %>%
  select(index, created_at, word) %>%
  mutate(created_at = ymd_hms(created_at))
trans_sports_full1

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
search_dictionary

# FILTER WITH DICTIONARY AND SAVE FILE ------------------------------------

# Apply filter
distress_dictionary <- trans_sports_full1 %>%
  # Just the stop words
  filter(word %in% search_dictionary) %>%
  mutate(distress_dictionary = rep(1, nrow(.))) %>%
  select(index, distress_dictionary)
distress_dictionary

# Save to file
write_csv(distress_dictionary, "data/features/trans_features_distress_dictionary.csv")
