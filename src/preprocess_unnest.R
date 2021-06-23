# Dependencies
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(SnowballC)
library(qdap)

# Import data
trans_sports_full <- readRDS("data/trans_sports_full.rds")

# Load stop words
data(stop_words)

# Keep personal pronoun to use "i" for personal distress
stop_words1 <- stop_words %>%
  filter(!word == "i")

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

# PRE-PROCESS -------------------------------------------------------------

# Pre-process text
trans_sports_full1 <- trans_sports_full %>%
  # Add index column
  mutate(index = row_number()) %>%
  # Keep original text
  mutate(text_original = text) %>%
  # Select columns for readability
  select(index, author_id, created_at, text, everything()) %>%
  # Transform date
  mutate(created_at = ymd_hms(created_at)) %>%
  # Remove Korean words
  filter(author_id != trans_sports_full[22488, ]$author_id) %>%
  # Remove white space and URLS
  mutate(
    text = str_replace_all(text, "\n", " "),
    text = str_replace_all(text, "(^|[:space:])(www|http)(.*?)($|[:space:])", " "),
    text = str_replace_all(text, "[:space:]{2,}", " ")
  ) %>%
  # Replace ' to ' and normalize text (unique(unlist(str_extract_all(trans_sports_full1$text, " [a-z]{1,}'t "))))
  mutate(text = str_replace_all(text, "'", "'")) %>%
  mutate(text = replace_abbreviation(text, replace = NULL, ignore.case = TRUE), 
         text = replace_contraction(text, replace = NULL, ignore.case = TRUE),
         text = str_replace_all(text, "cannot", "can not"),
         text = str_replace_all(text, "n't ", " not ")) %>%
  # remove mentions @XXX
  mutate(text = rm_tag(text)) %>%
  # Remove punctuation
  mutate(text = str_replace_all(text, "[[:punct:]]", " ")) %>%
  mutate(text = str_replace_all(text, "[0-9]+", " ")) %>%
  # Remove odd characters
  mutate(text = str_replace_all(text, "[\\W+]", " ")) %>%
  mutate(text = str_to_lower(text)) %>%
  mutate(text = str_replace_all(text, "[:space:]{2,}", " ")) %>%
  mutate(text = str_trim(text))
trans_sports_full1

# Transform to long format
trans_sports_tidy <- trans_sports_full1 %>%
  unnest_tokens(word, text) %>%
  # Remove stop words
  anti_join(stop_words) %>%
  anti_join(custom_stop_words) %>%
  # Reduce to word stem
  mutate(word = wordStem(word, language = "en")) %>%
  # Move main variables to the front
  select(index, author_id, created_at, word, everything())
trans_sports_tidy

# Prepare file for LIWC
trans_sports_liwc <- trans_sports_full1 %>%
  select(index:text, id, conversation_id:lang, in_reply_to_user_id, text_original)
trans_sports_liwc 

# EXPORT FILES ------------------------------------------------------------

# Save to file
saveRDS(trans_sports_full1, file = "data/trans_sports_clean_tweets.rds")
saveRDS(trans_sports_tidy, file = "data/trans_sports_clean_words.rds")

# Export to file for LIWC
write_csv(trans_sports_liwc, "data/trans_sports_liwc.csv")
