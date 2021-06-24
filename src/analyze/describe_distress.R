# Dependencies
library(tidyverse)

# Import
trans_distress_v2 <- read_csv("data/distress_tweets/trans_distress_v2_manual.csv")
trans_distress_v2

# Total count
nrow(trans_distress_v2)

# COMPARE -----------------------------------------------------------------

# Basic count - uncorrected
trans_distress_v2 %>%
  count(distress_tweet) %>%
  mutate(
    percent = n / nrow(trans_distress_v2)
  )

# Types of Tweet sources
trans_distress_v2 %>%
  distinct(source)

table(trans_distress_v2$source, trans_distress_v2$distress_tweet)

# Basic count - corrected
trans_distress_v2 %>%
  select(index, source, distress_tweet) %>%
  filter(source %in% c("Twitter for Android", "Twitter for iPad", 
                       "Twitter for iPhone", "Twitter Web App")) %>%
  count(distress_tweet) %>%
  mutate(
    sum = sum(n),
    percent = n / sum
  )

