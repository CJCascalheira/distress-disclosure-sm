# Resource
# https://www.tidytextmining.com/ngrams.html

# Dependencies
library(tidyverse)
library(lubridate)
library(tidytext)
library(SnowballC)

# Import data
trans_sports_full <- readRDS("data/trans_sports_full.rds")

# Load stop words
data(stop_words)

# Keep personal pronoun to use "i" for personal distress
stop_words1 <- stop_words %>%
  filter(!word == "i")

# Load sentiment library
afinn <- get_sentiments(lexicon = "afinn") 
afinn

# CLEAN DATA --------------------------------------------------------------

# Prepare data
trans_sports_full1 <- trans_sports_full %>%
  mutate(index = row_number()) %>%
  mutate(text_original = text) %>%
  select(index, author_id, created_at, text, everything()) %>%
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
  mutate(text = str_replace_all(text, "[\\W+]", " "))
trans_sports_full1

# Bigrams
trans_bigrams <- trans_sports_full1 %>%
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
  separate(bigram, c("word1", "word2"), sep = " ") %>%
  # Remove stop words
  filter(
    !(word1 %in% stop_words1$word),
    !(word2 %in% stop_words1$word)
  ) %>%
  # Move to front
  select(index:created_at, word1, word2, everything())
head(trans_bigrams)

# Trigrams
trans_trigrams <- trans_sports_full1 %>%
  unnest_tokens(trigram, text, token = "ngrams", n = 3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep = " ") %>%
  # Remove stop words
  filter(
    !(word1 %in% stop_words1$word),
    !(word2 %in% stop_words1$word),
    !(word3 %in% stop_words1$word)
  ) %>%
  # Move to front
  select(index:created_at, word1, word2, word3, everything())
head(trans_trigrams)

# ANALYZE BIGRAMS ---------------------------------------------------------

# Most frequent bigrams
trans_bigrams %>% 
  count(word1, word2, sort = TRUE)

# Bigrams starting with I - personal distress
bigrams_sentiment <- trans_bigrams %>% 
  # Only words starting with I
  filter(word1 == "i") %>%
  # Add count to data set to retain index
  add_count(word1, word2, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word2" = "word")) %>%
  # Most negative sentiment first
  arrange(value) %>%
  # Use next line just for detection; will need to remove if assigning features
  distinct(word1, word2, .keep_all = TRUE)

# Manual view of possible bigrams indicating distress
View(bigrams_sentiment)

# Good bigrams working coding into features

# ANALYZE TRIGRAMS --------------------------------------------------------

# Most frequent trigrams
trans_trigrams %>% 
  count(word1, word2, word3, sort = TRUE)

# Trigrams starting with I - personal distress
trigrams_sentiment <- trans_trigrams %>% 
  filter(word1 == "i") %>%
  add_count(word1, word2, word3, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, word3, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word2" = "word")) %>%
  left_join(afinn, by = c("word3" = "word")) %>%
  # Numerous trigrams first
  arrange(desc(n)) %>%
  # Use next line just for detection; will need to remove if assigning features
  distinct(word1, word2, word3, .keep_all = TRUE)

# Manual view of possible trigrams indicating distress
View(trigrams_sentiment)

# At least one notable trigram with decent count
# i feel bad