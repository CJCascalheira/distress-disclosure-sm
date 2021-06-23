# Resource
# https://www.tidytextmining.com/ngrams.html

# Dependencies
library(tidyverse)
library(lubridate)
library(tidytext)
library(SnowballC)

# Import data
trans_sports_wide <- readRDS("data/trans_sports_clean_tweets.rds")
nrow(trans_sports_wide)

# Load stop words
data(stop_words)

# Keep personal pronoun to use "i" for personal distress
stop_words1 <- stop_words %>%
  filter(!word %in% c("i", "am", "me"))

# Load sentiment library
afinn <- get_sentiments(lexicon = "afinn") 
afinn

# CLEAN DATA --------------------------------------------------------------

# Bigrams
trans_bigrams <- trans_sports_wide %>%
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
trans_trigrams <- trans_sports_wide %>%
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

# Good bigrams for coding into features
distress_bigrams <- trans_bigrams %>% 
  # Only words starting with I
  filter(word1 == "i") %>%
  # Remove "am"
  filter(word2 != "am") %>%
  # Add count to data set to retain index
  add_count(word1, word2, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word2" = "word")) %>%
  filter(
    # Keep bigrams with n > 0
    n > 0,
    # Keep negative sentiment
    value < 0
  ) %>%
  # Create new column
  mutate(distress_bigrams = rep(1, nrow(.))) %>%
  rename(bigram_word1 = word1, bigram_word2 = word2) %>%
  select(index, starts_with("bigram"), ends_with("bigrams"))
distress_bigrams

# Merge data for distress bigrams
trans_sports_wide1 <- trans_sports_wide %>%
  left_join(distress_bigrams) %>%
  # Replace missing values with zero
  mutate(
    distress_bigrams = if_else(is.na(distress_bigrams), 0, distress_bigrams)
  ) %>%
  distinct(index, .keep_all = TRUE)
trans_sports_wide1

# ANALYZE TRIGRAMS --------------------------------------------------------

# Most frequent trigrams
trans_trigrams %>% 
  count(word1, word2, word3, sort = TRUE)

# Trigrams starting with I - personal distress
trigrams_sentiment <- trans_trigrams %>% 
  filter(word1 == "i") %>%
  # Remove the word "am"
  filter(word2 != "am") %>%
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

# Trigrams starting with I am - personal distress
trigrams_i_am <- trans_trigrams %>% 
  filter(word1 == "i", word2 == "am") %>%
  add_count(word1, word2, word3, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, word3, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word3" = "word")) %>%
  filter(
    # Keep bigrams with n > 0
    n > 0,
    # Keep negative sentiment
    value < 0
  ) %>%
  # Create new column
  mutate(distress_trigrams_am = rep(1, nrow(.))) %>%
  rename(trigram_am1 = word1, trigram_am2 = word2, trigram_am3 = word3) %>%
  select(index, starts_with("trigram"), ends_with("_am"))
trigrams_i_am 

# Trigrams starting with I feel - personal distress
trigrams_i_feel <- trans_trigrams %>% 
  filter(word1 == "i", word2 == "feel") %>%
  add_count(word1, word2, word3, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, word3, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word3" = "word")) %>%
    filter(
      # Keep bigrams with n > 0
      n > 0,
      # Keep negative sentiment
      value < 0
    ) %>%
  # Create new column
  mutate(distress_trigrams_feel = rep(1, nrow(.))) %>%
  rename(trigram_feel1 = word1, trigram_feel2 = word2, trigram_feel3 = word3) %>%
  select(index, starts_with("trigram"), ends_with("_feel")) 
trigrams_i_feel

# Merge the datasets
trans_sports_wide2 <- trans_sports_wide1 %>%
  left_join(trigrams_i_am) %>%
  left_join(trigrams_i_feel) %>%
  # Replace missing values with zero
  mutate(
    distress_trigrams_am = if_else(is.na(distress_trigrams_am), 0, distress_trigrams_am),
    distress_trigrams_feel = if_else(is.na(distress_trigrams_feel), 0, distress_trigrams_feel)
  ) %>%
  distinct(index, .keep_all = TRUE)
trans_sports_wide2

# Trigrams ending in "me"
trans_trigrams %>% 
  filter(word3 == "me") %>%
  filter(word1 != "ct") %>%
  filter(word2 != "ks") %>%
  add_count(word1, word2, word3, sort = TRUE) %>%
  # Select columns to analyze
  select(index, created_at, word1, word2, word3, n) %>%
  # Detect negative sentiment
  left_join(afinn, by = c("word2" = "word")) %>%
  left_join(afinn, by = c("word1" = "word")) %>%
  arrange(value.x) %>%
  View()

# EXPORT DATASET WITH NEW FEATURES ----------------------------------------

# Double check row numbers
nrow(trans_sports_wide2)

# Save as RDS
saveRDS(trans_sports_wide2, file = "data/features/trans_features_ngrams.rds")
