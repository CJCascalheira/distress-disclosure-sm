# Dependencies
library(tidyverse)
library(tidytext)
library(stringr)
library(lubridate)
library(SnowballC)

# Import data
trans_sports_full <- readRDS("data/trans_sports_full.rds")

# Load sentiment library
afinn <- get_sentiments(lexicon = "afinn")

# Select relevant columns
trans_sports_full1 <- trans_sports_full %>%
  select(author_id, created_at, text) %>%
  mutate(created_at = ymd_hms(created_at))
