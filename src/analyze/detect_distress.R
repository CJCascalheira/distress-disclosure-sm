# Dependencies
library(tidyverse)
library(readr)

# Import
trans_clean_final1 <- read_rds("data/final_merged/trans_sports_final1.rds")
trans_clean_final2 <- read_rds("data/final_merged/trans_sports_final2.rds")
trans_clean_final3 <- read_rds("data/final_merged/trans_sports_final3.rds")
trans_clean_final4 <- read_rds("data/final_merged/trans_sports_final4.rds")

# Merge
trans_clean_final <- trans_clean_final1 %>%
  bind_rows(trans_clean_final2) %>%
  bind_rows(trans_clean_final3) %>%
  bind_rows(trans_clean_final4)

# DETECT DISTRESS DISCLOSURE ----------------------------------------------

# Detect distress based on features
trans_distress <- trans_clean_final %>%
  filter(
    distress_bigrams == 1 |
      distress_trigrams_am == 1 |
      distress_trigrams_feel == 1 |
      distress_dictionary == 1
  )

# First version - check work
trans_distress %>%
  distinct(index, .keep_all = TRUE)

# Save first version
write_rds(trans_distress, file = "data/distress_tweets/trans_distress_v1.rds")
