# Dependencies
library(tidyverse)
library(tidytext)
library(stringr)

# Import
trans_sports_tweets <- read_rds("data/trans_sports_clean_tweets.rds")
trans_sports_words <- read_rds("data/trans_sports_clean_words.rds")

# Tidy
trans_sports_tidy <- trans_sports_tweets %>%
  unnest_tokens("word", text) 

# SENTIMENT ANALYSIS ------------------------------------------------------

# afinn
afinn_sentiment <- get_sentiments("afinn")

afinn_trans_sports <- trans_sports_tidy %>% 
  inner_join(afinn_sentiment) %>%
  group_by(index) %>%
  summarise(value = sum(value)) %>%
  ungroup() %>%
  rename(afinn_score = value)

# bing
bing_sentiment <- get_sentiments("bing")

bing_trans_sports <- trans_sports_tidy  %>%
  inner_join(bing_sentiment) %>%
  group_by(index) %>%
  count(sentiment)

bing_trans_sports_wide <- bing_trans_sports %>% 
  spread(sentiment, n, fill = 0)

names(bing_trans_sports_wide) <- c("index", "bing_neg", "bing_pos")

# nrc 
nrc_sentiment <- get_sentiments("nrc")

nrc_trans_sports <- trans_sports_tidy %>%
  inner_join(nrc_sentiment) %>%
  group_by(index) %>%
  count(sentiment)

nrc_trans_sports_wide <- nrc_trans_sports %>% 
  spread(sentiment, n, fill = 0)

names(nrc_trans_sports_wide) <- c("index", "nrc_anger", "nrc_anticipation", "nrc_disgust", "nrc_fear",          "nrc_joy", "nrc_negative", "nrc_positive", "nrc_sadness", "nrc_surprise", "nrc_trust")

# SAVE TO FILE ------------------------------------------------------------

write_csv(afinn_trans_sports, "data/features/trans_features_afinn.csv")
write_csv(bing_trans_sports_wide, "data/features/trans_features_bing.csv")
write_csv(nrc_trans_sports_wide, "data/features/trans_features_nrc.csv")
