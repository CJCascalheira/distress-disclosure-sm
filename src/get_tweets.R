# Dependencies
library(academictwitteR)
library(tidyverse)

# Assign bearer token to a character object
bearer_token <- Sys.getenv("TWITTER_ACADEMIC_BEARER")

# Pull Tweets
tweets1 <- get_all_tweets(query = "anti-trans OR anti-transgender OR (trans sports) OR (trans ban)", 
                          start_tweets = "2021-03-11T00:00:00Z", # start date 
                          end_tweets = "2021-06-17T00:00:00Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "trans_sports_ban" # name of the resulting RDS file
)

## You will see a warning, don't worry! It is just warning us about data loss when we work with large amounts of 
## data

# This returns a data frame. 
class(tweets1)

# Let's take a peek at the first few rows  
head(tweets1)


# We have a lot of columns. If we want to investigate the data frame, 
# it might be useful to focus on some specific variables. For example, 
# we can  specifically check the text column to scan our keywords/hashtags.
head(tweets1$text) 

# By default, if we don't change the parameters, ths function returns the following 15 columns/fields. 
names(tweets1) # check the columns, by default it has 15 columns/fields 

## Also we can check the structure of the data frame
#str(tweets1) # you can also check the structure, but it is very long! 

############
## Example 2 

# In the second example, we will search for tweets containing the *sanctuary city* keyword OR the hashtag 
# *#sanctuary city* for a few days. The **OR logic** again works well for our query. Here, however, 
# we have a key word, `sanctuary city`, that we would like to capture as an exact phrase. More specifically, 
# suppose that we don't want tweets containing words sanctuary and city seperately in our tweets. 
# To match exact phrases in our queries, we need to wrap the phrase in double quotes and use single quotes 
# for the general query. 

## assigining to an object called tweets2,will be a data frame object  
tweets2 <- get_all_tweets(query = '"sanctuary city" OR #sanctuarycity', 
                          # Any tweet that contains either keyword/hashtag
                          # wrapping sanctuary city in double quotes, to capture the exact phrase! 
                          start_tweets = "2018-01-01T00:00:00Z", # start date 
                          end_tweets = "2018-01-02T00:00:00Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "immigrationtweets2" # name of the resulting RDS file 
                          # specificy a file argument - save an RDS format for all the tweets captured with the API call 
)

#Let's take a peek at the text column. 
head(tweets2$text)

# What happens if we don't wrap the phrase *sanctuary city* in double quotes? 
# If we write ``"sanctuary city OR #sanctuarycity"``, then the query will search for tweets containing the 
# words *sanctuary* AND *city* OR the hashtag *#sanctuarycity*. You can see the difference from the previous 
# query by running the code chunk below. 

# ## assigining to an object called tweets_2,will be a data frame object  
# tweets2_diff <- get_all_tweets(query = "sanctuary city OR #sanctuarycity", 
#                                # Any tweet that contains either key word/hashtag
#                                # wrapping sanctuary city in double quotes, matches the exact phrase! 
#                                start_tweets = "2018-01-01T00:00:00Z", # start date 
#                                end_tweets = "2018-01-02T00:00:00Z", # end date
#                                bearer_token = bearer_token, # bearer token to access Twitter API 
#                                file = "immigrationtweets2_diff" # name of the resulting RDS file 
#                                #specificy a file argument - save an RDS format for all the tweets captured with the API call 
# )
# head(tweets2_diff$text) 
# tail(tweets2_diff$text)

# If we want to collect tweets that contain both the keyword *sanctuary city* AND the hashtag  *#sanctuarycity*,
# then we will use the **AND logic**. How to use this logic with this function? Putting a space between 
# our keywords will result in the AND logic. 

############
## Example 3

# In our third example, we will collect tweets that contain both the keyword *sanctuary city* as a phrase 
# AND the keyword *Chicago*. Therefore, we will use the **AND logic**. How to use this
# logic with this function? Putting a space between our keywords will result in the AND logic. 

## assigining to an object called tweets3,will be a data frame object  
## here we are looking for a limited time, this is okay, we won't crush API 
tweets3 <- get_all_tweets(query = '"sanctuary city" Chicago', 
                          # Any tweet that contains both keyword/hashtag 
                          # wrapping sanctuary city in double quotes, matches the exact phrase
                          start_tweets = "2018-01-01T00:00:00Z", # start date 
                          end_tweets = "2018-01-10T00:00:00Z", # end date
                          bearer_token = bearer_token, # bearer token to access Twitter API 
                          file = "immigrationtweets3" # name of the resulting RDS file 
                          # specificy a file argument - save an RDS format for all the tweets captured with the API call 
)

# When to use this data storage? This works well if we want to try out something for a short 
# time period, or if we want to experiment with query building. 

# Now, we will move on with the second way of data stroge. 
# This will be more appropriate if our aim is to collect a large set of tweets. 

# ---------------------------------------- #
# 2. Storage in JSON with a data path (larger tasks)

# Let's say we are interested in tweets about immigration with the key word *sanctuary city* OR 
# the hashtag *#sanctuary city* over a longer time period, say 10 years. This API call will run at 
# least several days by paginating over each Twitter page that match our query! For such tasks, 
# the creators of the package suggests to store the data as a series of JSON files to mitigate data loss. 
# In practice, we just need to modify our previous code chunks a little bit. We will add two additional 
# arguments to the ``get_all_tweets`` function: 1) name our data directory where we want to save our data using
# the data_path argument, 2) don't bind the tweets in a single data frame by setting bind_tweets argument to FALSE.
# This is because our data will be stored in multiple JSON files in a folder. Each file will contain approximately 
# up to 500 tweets. We will also remove the file argument we had before. Lastly, we will not assign the function 
# to any object since the function won't return anything here! Let's illusrate with an example.

get_all_tweets(query = '"sanctuary city" OR #sanctuarycity', 
               # Any tweet that contains either key word/hashtag
               # wrapping sanctuary city in double quotes, matches the exact phrase! 
               start_tweets = "2018-01-01T00:00:00Z", # start date 
               end_tweets = "2018-01-02T00:00:00Z", # end date
               bearer_token = bearer_token, # bearer token to access Twitter API 
               data_path = "immigration_tweets/", # data path where JSON files will be stored 
               bind_tweets = FALSE) # don't bind the tweets


# The collected tweets are now in our data path in the folder *immigration_tweets* as multiple JSON files.
# Let's check how many JSON files we have. 
list.files("immigration_tweets") # total 4 json files and one query text file


# How to bind our tweets that are stored in multiple JSON files?
# Our aim to convert these JSON files into a format that we can use for our analysis. 
# Good news! There is a convenient function to do this. We can use a function called ``bind_tweet_jsons``
immig_data <- bind_tweet_jsons(data_path = "immigration_tweets")

#Now, we have a data frame.
class(immig_data)

## let's take a peek quickly 
head(immig_data$text)

#The only thing that changes is the way data is stored
#Other than that, everything is the same with our previous examples. 
names(immig_data)

# ---------------------------------------- #
## Collect User Tweets 

# To collect tweets of a user or set of users between specified date ranges we can use the function ``
# get_user_tweets``. 
# Let's collect tweets of some U.S. governors between March, 2021 and June, 2021. 

#To make our life easier, we can assign multiple users we are interested in to an object called users.
users <- c("GavinNewsom", "GovWhitmer", "GovLauraKelly", "OregonGovBrown")

get_user_tweets(users, # users object 
                start_tweets = "2021-03-01T00:00:00Z", # start date
                end_tweets = "2021-06-01T00:00:00Z", # end date
                bearer_token = bearer_token, # bearer token 
                data_path = "governors_tweets/", # data path where JSON files will be stored 
                bind_tweets = FALSE) # don't bind the tweets


# These tweets are now in our data path as multiple JSON files in the folder 
# *governors_tweets*. Let's bind these JSON files.
## can check the files included
list.files("governors_tweets") # total 8 files 
## read JSON Files into a data frame 
gov_data <- bind_tweet_jsons(data_path = "governors_tweets")
## now we have a data frame
class(gov_data)

#Let's take a peek at our data frame
## let's take a peak quickly 
gov_data[1,]

# Again, we have 15 columns in total 
names(gov_data)

#This function only returns author IDs, but not the name of user profiles
table(gov_data$author_id)

# We can figure out which author ID belongs to which governor. But, there is an easier way to 
# retrieve governor names. 
# We can use the get_user_profile() function to retrieve the user profiles. 

## assign an object to retrieve and store author ids
authors <- gov_data$author_id
## use this obect to get user profiles by activating your bearer token 
authors_profile <- get_user_profile(authors, bearer_token)
names(authors_profile)

#If we are only interested in names of the governors, 
# we can subset the name column and combine this with our original governors data as a column. 
authors_profile2 <- authors_profile %>% select(name)
gov_data <- cbind(gov_data, authors_profile2)
table(gov_data$name)

#########
## Plotting Tweets 

# In the academictwitteR package, currently, there is no build-in function like ts_plot (rtweet package) 
# for plotting tweets as a time series graph. But, we can use ggplot! 
## we already loaded tidyverse (includes ggplot2)
head(gov_data$created_at)
# first, need to convert the created_at variable, let's name our new variable date
## there are many different ways to do this conversion
gov_data$date <- as.Date(gov_data$created_at)
head(gov_data$date)
#class(gov_data$date)

## Now we need to collapse the tweets at the day level for each governor 
dat_collapsed_daily <- gov_data %>% group_by(name, date) %>% summarize(n())
dat_collapsed_daily
dat_collapsed_daily <- dat_collapsed_daily %>% mutate(frequency = `n()`)
dat_collapsed_daily
## time to plot our tweets 
dat_collapsed_daily %>% 
  # line below, core component of our plot: `data` and `mapping`. 
  ggplot(aes(x = date, y = frequency, group = name, color = name)) + geom_line() +  
  # positin new scales for date on the x-axis  
  scale_x_date(date_breaks = "15 days", date_labels = "%b-%d-%y") +
  # give a title 
  ggtitle("Tweet Frequency from Four Governors",subtitle = "Tweet counts aggregated daily") +
  # change the name of the legend
  labs(color='Governors') +
  # change legend labels/colors
  scale_color_manual(labels = c("Newsom", "Whitmer", "Brown", "Kelly"), values = c("aquamarine3", "red", "orange",
  "purple")) + 
  # make the title bold, change the names of x and y axis labels 
  theme(plot.title = element_text(face = "bold")) + ylab("Frequency") + xlab("Weeks") + 
  # adjust the axis text 
  theme(axis.text.x=element_text(angle=60, hjust=1)) + 
  # dark-on-light ggplot2 theme (??ggtheme)
  theme_bw()


# ---------------------------------------- #
## Getting Videos and Images 

# Functions ``get_video_tweets`` and ``get_image_tweets`` allow to collect tweets with videos or images 
# for specific keywords and hashtags between specified date ranges.

# Tweets with videos contain native Twitter videos, uploaded directly to Twitter.
# The function do not match videos created with other applications such as Periscope, or 
# Tweets with links to other video hosting sites. Below is an example for videos containing the hashtag 
# *#BlackLivesMatter*. This time, let's use the first way of storing tweets. 
  
blm_videos <- get_video_tweets(query = "#BlackLivesMatter",
                               start_tweets = "2017-01-01T00:00:00Z",
                               end_tweets = "2017-01-05T00:00:00Z",
                               bearer_token)


#Let's check how texts of tweets look like
head(blm_videos$text) # they all contain an url link 


# We can also collect tweets that contain (a recognized URL to) an image. 
# However, currently, this functionality of the package is not working properly. 
# Therefore, it will give an error. 

# blm_images <- get_image_tweets("#BlackLivesMatter",
#                                start_tweets = "2017-01-01T00:00:00Z",
#                                end_tweets = "2017-01-05T00:00:00Z",
#                                bearer_token)


# -------------------------------
## Filtering Tweets by location
# -------------------------------
# 
# There are two types of geographical metadata when we are collecting Twitter data. 
# 
# **1. Tweet location:** Available when user shares location at time of Tweet.
# **2. Account Location:** Based on the 'home' location provided by user in their public profile. This is a  
# free-form character field and may or may not contain metadata that can be geo-referenced. These are described 
# separately in the next two sections.
# 
# This [link](https://developer.twitter.com/en/docs/tutorials/filtering-tweets-by-location) provides
# more information for filtering tweets by location. We will discuss some examples for collecting Twitter
# data by tweet location in the next part on building queries. 

# -------------------------------
## Building Queries 
# -------------------------------

# -------------------------------
## Manual Queries 

# Twitter Academic API can accept fairly complex queries (a query can be 1024 characters long), 
# we can add many distinct paramaters. 
# This [link](https://developer.twitter.com/en/docs/twitter-api/tweets/search/integrate/build-a-query) 
# provides a detailed overview of queries with some examples.

#####
## Tweets from a country 
# Suppose that we are interested in tweets containing the hashtag *#BlackLivesMatter* 
#   from a specific country, United States, that are only English. We also want to exclude retweets. 
# For collecting tweets by country, we can use the ``place_country`` operator.
  
blm_query1 <- get_all_tweets("#BlackLivesMatter place_country:US lang:en -is:retweet",
                             start_tweets = "2020-01-01T00:00:00Z",
                             end_tweets = "2020-03-01T00:00:00Z",
                             bearer_token
)

#Now, our place_id column is not empty anymore. But, we get way less tweets
# than we would have without the location.  
head(blm_query1$geo)

# This is how our previous data frames without geometadata looks like
head(tweets1$geo)

#####
## Tweets from a place 

#We can collect tweets from a specific place using the ``place`` operator, e.g. city, borough 
#Let's say we want to collect tweets in English from California.
## What if we are interested in a specific location? Say, California
blm_query2 <- get_all_tweets("#BlackLivesMatter place:California lang:en -is:retweet", 
                             start_tweets = "2020-01-01T00:00:00Z",
                             end_tweets = "2020-03-01T00:00:00Z",
                             bearer_token)

## here, we need to investigate this further before we rely on this query, 
# e.g. we might represent the same place with different names, e.g. CA etc. (gives 199 tweets!)..

#####
## Bounding-box 
# We can also use an operator called ``bounding box``.
# This allows you to specify a 4-sided geographic area and match Tweets containing 
# Tweet-specific location data that fall within that area. Below is an illustration 

## bounding_box:[west_long south_lat east_long north_lat]
# -73.9393484455,40.785171272,-73.8185225894,40.9007276281
## each side of the box is up to 25 miles in length.
blm_query3 <- get_all_tweets("#BlackLivesMatter bounding_box:[-73.93 40.78 -73.82 40.90] lang:en", 
                             start_tweets = "2020-01-01T00:00:00Z",
                             end_tweets = "2020-4-01T00:00:00Z",
                             bearer_token)


#####
## Point-radius 

# Another operator for collecting tweets by location is the ``point_radius`` operator. 
# This allows to specify a circular geographic area and match Tweets containing Tweet-specific
# location data that fall within that area. For example, we can look at the tweets containing the word
# *insurrection* in a 25 miles radius around the Capitol on January 6. 

## point_radius:[lon lat radius]
## radius must be less than 25 miles! 
capitol_radius <- get_all_tweets("insurrection point_radius:[-77.009017 38.890550 25mi]",
                                 start_tweets = "2021-01-05T00:00:00Z",
                                 end_tweets = "2021-01-07T00:00:00Z",
                                 bearer_token)


## take a peek at geo info 
head(capitol_radius$geo)

# -------------------------------
## Query Builder 

#Manual approach requires to know the syntax of Twitter API queries. Alternatively, 
#we can also benefit from the academictwitteR package's own query builder, ``build_query`` function. 


#We can build queries for the following queries. 
args(build_query)

# The example below shows how to build a query to collect tweets containing the *#BlackLivesMatter* hashtag 
#   by country. This function will build us a tweet query. 
#   We will then use this as an input in the workhorse function get_all_tweets as query parameter.
blm_querybuild <- build_query(query = "#BlackLivesMatter", country = "US", lang = "en")
blm_querybuild

blmquery_tweets <- get_all_tweets(blm_querybuild, # insert query parameter from the build quey function 
                                  start_tweets = "2020-01-01T00:00:00Z",
                                  end_tweets = "2020-03-01T00:00:00Z",
                                  bearer_token
)

# -------------------------------
## Interruption and Continuation
# -------------------------------

# When making long API calls, there might be some interruptions. 
# ``resume_collection`` function resumes a previous interrupted collection session. 

resume_collection(data_path = "immigration_tweets", bearer_token)

# Because we don't experienced any interruption while collecting 
# immigration tweets, the function won't give us any new tweets. 

# We can also update_collection our collection. 
# If we want to continue previous collection session with a new end date. 

# update_collection(data_path = "governors_tweets", end_tweets = "2021-06-10T00:00:00Z",
                  # bearer_token)

# ---------------------------------------------


