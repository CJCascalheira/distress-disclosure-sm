# Dependencies
library(tidyverse)

# Import
trans_distress_v2 <- read_csv("data/distress_tweets/trans_distress_v2_manual.csv")
trans_distress_v2

# Total count
nrow(trans_distress_v2)

# BASIC COMPARISON --------------------------------------------------------

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

# DESCRIBE THE GROUPS -----------------------------------------------------

# Summary variables
trans_distress_v2 %>%
  group_by(distress_tweet) %>%
  summarize(
    analytic = mean(Analytic),
    clout = mean(Clout),
    authentic = mean(Authentic),
    tone = mean(Tone)
  )

# Cognitive features
trans_distress_v2 %>%
  group_by(distress_tweet) %>%
  summarize(
    # swear = mean(swear),
    focus_future = mean(focusfuture),
    focus_present = mean(focuspresent),
    discrep = mean(discrep),
    certain = mean(certain),
    insight = mean(insight)
  )

# Separate data sets
distress_no <- trans_distress_v2 %>%
  filter(distress_tweet == 0)

distress_yes <- trans_distress_v2 %>%
  filter(distress_tweet == 1)

# Running t tests
t.test(distress_no$Analytic, distress_yes$Analytic)
t.test(distress_no$Clout, distress_yes$Clout)
t.test(distress_no$Authentic, distress_yes$Authentic)
t.test(distress_no$Tone, distress_yes$Tone)
t.test(distress_no$focusfuture, distress_yes$focusfuture)
t.test(distress_no$focuspresent, distress_yes$focuspresent)
t.test(distress_no$discrep, distress_yes$discrep)
t.test(distress_no$certain, distress_yes$certain)
t.test(distress_no$insight, distress_yes$insight)

# TOP WORDS ---------------------------------------------------------------

top_words %>%
  slice(1:20) %>%
  ggplot(aes(x=reorder(word, -n), y=n, fill=word))+
  geom_bar(stat="identity")+
  theme_minimal()+
  theme(axis.text.x = 
          element_text(angle = 60, hjust = 1, size=13))+
  theme(plot.title = 
          element_text(hjust = 0.5, size=18))+
  ylab("Frequency")+
  xlab("")+
  ggtitle("Most Frequent Words in anti-trans sports ban Tweets")+  
  guides(fill=FALSE)
