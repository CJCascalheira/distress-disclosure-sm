# Dependencies
library(tidyverse)
library(tidytext)
library(stringr)

# Import data
trans_sports <- readRDS("data/trans_sports_clean.rds")

# Load sentiment library
afinn <- get_sentiments(lexicon = "afinn") 
afinn

# Word stems only, otherwise we will miss alot 
afinn_1 <- afinn %>%
  mutate(word = wordStem(word, language = "en")) %>%
  distinct(word, .keep_all = TRUE)
