# Dependencies
library(tidyverse)

# Import data
trans_sports_ban0 <- readRDS("data/trans_sports_ban0.rds") %>%
  as_tibble()

trans_sports_ban1a <- readRDS("data/trans_sports_ban1a.rds") %>%
  as_tibble()

trans_sports_ban1b <- readRDS("data/trans_sports_ban1b.rds") %>%
  as_tibble()

trans_sports_ban2 <- readRDS("data/trans_sports_ban2.rds") %>%
  as_tibble()

trans_sports_ban3 <- readRDS("data/trans_sports_ban3.rds") %>%
  as_tibble()

# Merge Twitter data
trans_sports_full <- trans_sports_ban0 %>%
  bind_rows(trans_sports_ban1a) %>%
  bind_rows(trans_sports_ban1b) %>%
  bind_rows(trans_sports_ban2) %>%
  bind_rows(trans_sports_ban3)

# Save to file
saveRDS(trans_sports_full, file = "data/trans_sports_full.rds")
