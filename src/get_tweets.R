# Dependencies
library(academictwitteR)
library(tidyverse)

# Assign bearer token to a character object
bearer_token <- Sys.getenv("TWITTER_ACADEMIC_BEARER")

# Build query
trans_query <- build_query(
  query = 'trans sports',
  is_retweet = FALSE,
  lang = "en"
)

# Pull tweets - March 11th - April 10th 
tweets0 <- get_all_tweets(query = trans_query, 
                          start_tweets = "2021-03-11T00:00:00Z", # start date 
                          end_tweets = "2021-04-10T23:59:59Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "data/trans_sports_ban0" # name of the resulting RDS file
)

# Pull tweets - April 11th - April 24th 
tweets1_a <- get_all_tweets(query = trans_query, 
                          start_tweets = "2021-04-11T00:00:00Z", # start date 
                          end_tweets = "2021-04-24T23:59:59Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "data/trans_sports_ban1a" # name of the resulting RDS file
)

# Pull tweets - April 24th - May 10th 
tweets1_b <- get_all_tweets(query = trans_query, 
                            start_tweets = "2021-04-25T00:00:00Z", # start date 
                            end_tweets = "2021-05-10T23:59:59Z", # end date
                            bearer_token = bearer_token, # bearer token to access Twitter API 
                            file = "data/trans_sports_ban1b" # name of the resulting RDS file
)

# Pull tweets - May 11th - June 10th
tweets2 <- get_all_tweets(query = trans_query, 
                          start_tweets = "2021-05-11T00:00:00Z", # start date 
                          end_tweets = "2021-06-10T23:59:59Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "data/trans_sports_ban2" # name of the resulting RDS file
)

# Pull tweets - June 11th - June 22nd
tweets3 <- get_all_tweets(query = trans_query, 
                          start_tweets = "2021-06-11T00:00:00Z", # start date 
                          end_tweets = "2021-06-22T00:00:00Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "data/trans_sports_ban3" # name of the resulting RDS file
)
