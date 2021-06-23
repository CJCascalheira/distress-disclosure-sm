# Dependencies
library(tidyverse)
library(readr)

# Import
trans_features_ngrams <- readRDS("data/features/trans_features_ngrams.rds")
liwc <- read_csv("data/features/trans_features_liwc.csv")
trans_dictionary <- read_csv("data/features/trans_features_distress_dictionary.csv")
afinn_trans_sports <- read_csv("data/features/trans_features_afinn.csv")
bing_trans_sports_wide <- read_csv("data/features/trans_features_bing.csv")
nrc_trans_sports_wide <- read_csv("data/features/trans_features_nrc.csv")
trans_sports_tweets <- read_rds("data/trans_sports_clean_tweets.rds")
trans_sports_words <- read_rds("data/trans_sports_clean_words.rds")

# PREPARE MERGE -----------------------------------------------------------

# Merge sentiments
trans_sports_tweets_merged <-  merge(trans_sports_tweets, afinn_trans_sports, by = "index", all = TRUE)

trans_sports_tweets_merged$afinn_score[which(is.na(trans_sports_tweets_merged$afinn_score))] <- 0

trans_sports_tweets_merged_1 <-  merge(trans_sports_tweets_merged, bing_trans_sports_wide, by = "index", all= TRUE)

trans_sports_tweets_merged_1$bing_neg[which(is.na(trans_sports_tweets_merged_1$bing_neg))] <- 0

trans_sports_tweets_merged_1$bing_pos[which(is.na(trans_sports_tweets_merged_1$bing_pos))] <- 0

trans_sports_tweets_merged_2 <-  merge(trans_sports_tweets_merged_1, nrc_trans_sports_wide, by = "index", all = TRUE)

trans_sports_tweets_merged_2$nrc_anger[which(is.na(trans_sports_tweets_merged_2$nrc_anger))] <- 0

trans_sports_tweets_merged_2$nrc_anticipation[which(is.na(trans_sports_tweets_merged_2$nrc_anticipation))] <- 0

trans_sports_tweets_merged_2$nrc_disgust[which(is.na(trans_sports_tweets_merged_2$nrc_disgust))] <- 0

trans_sports_tweets_merged_2$nrc_fear[which(is.na(trans_sports_tweets_merged_2$nrc_fear))] <- 0

trans_sports_tweets_merged_2$nrc_joy[which(is.na(trans_sports_tweets_merged_2$nrc_joy))] <- 0

trans_sports_tweets_merged_2$nrc_negative[which(is.na(trans_sports_tweets_merged_2$nrc_negative))] <- 0

trans_sports_tweets_merged_2$nrc_positive[which(is.na(trans_sports_tweets_merged_2$nrc_positive))] <- 0

trans_sports_tweets_merged_2$nrc_sadness[which(is.na(trans_sports_tweets_merged_2$nrc_sadness))] <- 0

trans_sports_tweets_merged_2$nrc_surprise[which(is.na(trans_sports_tweets_merged_2$nrc_surprise))] <- 0

trans_sports_tweets_merged_2$nrc_trust[which(is.na(trans_sports_tweets_merged_2$nrc_trust))] <- 0

# Convert to tiblle
as_tibble(trans_sports_tweets_merged_2)

# Prepare LIWC
liwc_re <- liwc[-1 ,c(1,10:102)] %>%
  rename(index = A) %>%
  mutate(index = as.numeric(index))
head(liwc_re)

# Merge the file
trans_sports_tweets_merged_final <-  merge(trans_sports_tweets_merged_2, 
                                           liwc_re, by = "index", all=TRUE) %>%
  as_tibble()

# Clean ngrams
trans_ngrams_clean <- trans_features_ngrams %>%
  select(index, created_at, distress_bigrams, distress_trigrams_am, 
         distress_trigrams_feel, starts_with("bigram"), starts_with("trigram"))
trans_ngrams_clean

# Clean LIWC
trans_liwc_clean <- liwc[-1, ] %>%
  rename(
    index = A,
    author_id = B,
    created_at = C,
    text = D,
    id = E,
    conversation_id = "F",
    possibily_sensitive = G,
    lang = H,
    in_reply_to_user_id = I,
    text_original = J
  ) %>%
  # Drop created_at, since it is character
  select(-created_at) %>%
  # Convert index to number
  mutate(index = as.numeric(index))
trans_liwc_clean

# Export clean LIWC
# write_csv(trans_liwc_clean, "data/trans_liwc_clean.csv")

# MERGE DATA --------------------------------------------------------------

# Full dataset
trans_clean_final <- trans_sports_tweets_merged_final %>%
  left_join(trans_ngrams_clean, by = "index") %>%
  left_join(trans_dictionary, by = "index") %>%
  # Add zeros to the distress dictionary
  mutate(
    distress_dictionary = if_else(is.na(distress_dictionary), 0, distress_dictionary)
  ) %>%
  distinct(index, .keep_all = TRUE)

# Prepare to split data
nrow(trans_clean_final) / 4
20295
20295 * 2
20295 * 3

# Split datasets to upload to GitHub
trans_clean_final1 <- trans_clean_final[1:20295,]
trans_clean_final2 <- trans_clean_final[20296:40590,]
trans_clean_final3 <- trans_clean_final[40591:60885,]
trans_clean_final4 <- trans_clean_final[60886:81179,]

# Check splitting
nrow(trans_clean_final1) +
  nrow(trans_clean_final2) +
  nrow(trans_clean_final3) +
  nrow(trans_clean_final4)

# Write to file
write_rds(trans_clean_final1, "data/final_merged/trans_sports_final1.rds")
write_rds(trans_clean_final2, "data/final_merged/trans_sports_final2.rds")
write_rds(trans_clean_final3, "data/final_merged/trans_sports_final3.rds")
write_rds(trans_clean_final4, "data/final_merged/trans_sports_final4.rds")
