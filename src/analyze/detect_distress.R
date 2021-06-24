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
trans_distress_1 <- trans_clean_final %>%
  filter(
    distress_bigrams == 1 |
      distress_trigrams_am == 1 |
      distress_trigrams_feel == 1 |
      distress_dictionary == 1
  )

# First version - check work
trans_distress_1 %>%
  distinct(index, .keep_all = TRUE)

# Save first version
# write_rds(trans_distress_1, file = "data/distress_tweets/trans_distress_v1.rds")

# Detect distress
trans_distress_2 <- trans_clean_final %>%
  # Higher numbers are associated with more honest, personal, disclosing text
  filter(Authentic > 50) %>%
  # Lower numbers reveals greater anxiety, sadness, or hostility
  filter(Tone < 50) %>%
  filter(
    distress_bigrams == 1 |
      distress_trigrams_am == 1 |
      distress_trigrams_feel == 1 |
      distress_dictionary == 1
  )
View(trans_distress_2)

# Save second version
# write_rds(trans_distress_2, file = "data/distress_tweets/trans_distress_v2.rds")

# Save second version - CSV
trans_distress_2 %>%
  # Remove list objects
  discard(is.list) #%>%
  # Write to file, then manually code
  #write_csv(file = "data/distress_tweets/trans_distress_v2.csv")
