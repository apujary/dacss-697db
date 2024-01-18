library(rtweet)
library(leaflet)
library(quanteda)
library(readr)
library(dplyr)
library(tidyr)
library(syuzhet)
library(lubridate)
library(tidytext)

# collect tweets
auth <- rtweet_app()

# nov 28th-dec 3rd
tweets <- search_tweets("#fakenews", n = 10000, token=auth) 
readr::write_csv(tweets, "#fakenews_tweets.csv")

# dec 4th-8th
tweets1 <- search_tweets("#fakenews", n = 10000, token=auth) 
readr::write_csv(tweets1, "#fakenews_tweets1.csv")

#-----------
# TWEETS

# get geocodes - tweets
geocodes <- lat_lng(tweets)

geocodes <- geocodes[!is.na(geocodes$lat),] 

readr::write_csv(geocodes,"#fakenews_geocodes.csv") 

# extract sentiments - tweets

## standardizing timestamps
tweets$created_at <- ymd_hms(tweets$created_at) #standardizing date-time format (YMD-HMS)
tweets$created_at <- with_tz(tweets$created_at,"America/New_York") #converting timezone information
tweets$created_date <- as.Date(tweets$created_at) #new column containing only date

tweets$date_label <- as.factor(tweets$created_date)

## creating daily count - tweets
daily_count <- tweets %>%
  group_by(date_label) %>%
  summarise(avg_rt = mean(retweet_count), avg_fav = mean(favorite_count), tweet_count = length(unique(id_str))) 

daily_count <- daily_count %>% pivot_longer(cols = -c(date_label), names_to = "variable", values_to = "value")

## aggregating by sentiments - tweets
tweets_sentiment <- get_nrc_sentiment(tweets$full_text)  ## scoring the tweets' sentiments
tweets_sentiment <- cbind(tweets, tweets_sentiment) 

# grouping the data by date and finding the mean scores of each sentiment - tweets
sentiments <- tweets_sentiment %>% 
  group_by(date_label) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust),
            negative = mean(negative),
            positive = mean(positive)) 

sentiments <- sentiments %>% pivot_longer(cols = -c(date_label), names_to = "variable", values_to = "value")
sentiments$date_label <- as.Date(sentiments$date_label)
readr::write_csv(sentiments,"#fakenews_sentiments.csv")
#----------
#TWEETS1

# get geocodes - tweets1
geocodes1 <- lat_lng(tweets1)

geocodes1 <- geocodes1[!is.na(geocodes1$lat),] 

readr::write_csv(geocodes1,"#fakenews_geocodes1.csv") 

# extract sentiments - tweets1

## standardizing timestamps
tweets1$created_at <- ymd_hms(tweets1$created_at) #standardizing date-time format (YMD-HMS)
tweets1$created_at <- with_tz(tweets1$created_at,"America/New_York") #converting timezone information
tweets1$created_date <- as.Date(tweets1$created_at) #new column containing only date

tweets1$date_label <- as.factor(tweets1$created_date)

## creating daily count - tweets1
daily_count1 <- tweets1 %>%
  group_by(date_label) %>%
  summarise(avg_rt = mean(retweet_count), avg_fav = mean(favorite_count), tweet_count = length(unique(id_str))) 

daily_count1 <- daily_count1 %>% pivot_longer(cols = -c(date_label), names_to = "variable", values_to = "value")

## aggregating by sentiments - tweets1
tweets_sentiment1 <- get_nrc_sentiment(tweets1$full_text)  ## scoring the tweets' sentiments
tweets_sentiment1 <- cbind(tweets1, tweets_sentiment1) 

# grouping the data by date and finding the mean scores of each sentiment - tweets1
sentiments1 <- tweets_sentiment1 %>% 
  group_by(date_label) %>% 
  summarise(anger = mean(anger), 
            anticipation = mean(anticipation), 
            disgust = mean(disgust), 
            fear = mean(fear), 
            joy = mean(joy), 
            sadness = mean(sadness), 
            surprise = mean(surprise), 
            trust = mean(trust),
            negative = mean(negative),
            positive = mean(positive)) 

sentiments1 <- sentiments1 %>% pivot_longer(cols = -c(date_label), names_to = "variable", values_to = "value")
sentiments1$date_label <- as.Date(sentiments1$date_label)
readr::write_csv(sentiments1,"#fakenews_sentiments1.csv")


# combine 'tweets' and 'tweets1'
tweets0 <- bind_rows(tweets,tweets1)
readr::write_csv(tweets0,"#fakenews_tweets_all.csv")

# combine 'geocodes' and 'geocodes1'
geocodes0 <- bind_rows(geocodes,geocodes1)
readr::write_csv(geocodes0,"#fakenews_geocodes_all.csv")

# combine 'sentiments' and 'sentiments1'
sentiments0 <- bind_rows(sentiments, sentiments1)
sentiments0$date_label <- as.Date(sentiments0$date_label)
readr::write_csv(sentiments0,"#fakenews_sentiments_all.csv")

# identifying key sentimental terms identified in the tweets
full_text_clean <-  tweets0 %>%
  select(full_text) %>%
  unnest_tokens(word, full_text)
sentiment_word_counts <- full_text_clean %>%
  inner_join(get_sentiments("nrc")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()
readr::write_csv(sentiment_word_counts,"#fakenews_sentiments_words.csv")





