# Dependencies
library(tidyverse)

# Import
trans_features_ngrams <- readRDS("data/features/trans_features_ngrams.rds")
trans_liwc <- read_csv("data/features/trans_features_liwc.csv")

# PREPARE MERGE -----------------------------------------------------------

# Clean LIWC
trans_liwc_clean <- trans_liwc[-1, ] %>%
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
  )
trans_liwc_clean
