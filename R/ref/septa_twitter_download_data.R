library(twitteR)
library(lubridate)
library(dplyr)
library(stringr)
library(ggplot2)


# functions

read_tweets <- function(accts) {
  require(twitteR)
  require(dplyr)
  # reads tweets from accounts and bind them together into one dataframe
  tab <- data.frame()
  for (i in accts) {
    Sys.sleep(5)
    tw <- userTimeline(i, 
                       excludeReplies = TRUE, 
                       since = '2016-01-01', 
                       n = 3200)
    tw.df <- twListToDF(tw)
    tab <- bind_rows(tab, tw.df)
  }
  return(tab)
}

# authenticate
token <- readRDS('.httr-oauth')

setup_twitter_oauth(
  consumer_key = 
    token$`5e9476eccc4723dd7b23bc4bdf755ab6`$app$key,
  consumer_secret = 
    token$`5e9476eccc4723dd7b23bc4bdf755ab6`$app$secret,
  access_token = 
    token$`5e9476eccc4723dd7b23bc4bdf755ab6`$credentials$oauth_token,
  access_secret = 
    token$`5e9476eccc4723dd7b23bc4bdf755ab6`$credentials$oauth_token_secret
)

# list of septa route-specific twitter accounts

lines <- c(
  "@SEPTA_TRL_10", 
  "@SEPTA_TRL_11",
  "@SEPTA_TRL_13", 
  "@SEPTA_TRL_15", 
  "@SEPTA_TRL_34",
  "@SEPTA_TRL_36", 
  "@SEPTA_TRL_101", 
  "@SEPTA_TRL_102", 
  "@SEPTA_AIR", 
  "@SEPTA_CHE",
  "@SEPTA_CHW", 
  "@SEPTA_CYN", 
  "@SEPTA_FOX", 
  "@SEPTA_DOY", 
  "@SEPTA_NOR",
  "@SEPTA_ELW", 
  "@SEPTA_PAO", 
  "@SEPTA_TRE", 
  "@SEPTA_WAR", 
  "@SEPTA_WTR",
  "@SEPTA_WIL", 
  "@SEPTA_MFL", 
  "@SEPTA_BSL", 
  "@SEPTA_NHSL",
  "@SEPTA_Bus"
)

tweets <- read_tweets(lines)

# first figure out what line it was associted with
tweets$line <- str_extract(tweets$screenName, "(?<=_).*$")

# now figure out the day of the week and time of day

tweets$date <- as.Date(tweets$created)

tweets$wday <- wday(tweets$created, label = TRUE)

tweets$time <- ymd_hms(tweets$created, tz = 'America/New_York')

tweets$h.m <- paste0(
  hour(tweets$time),
  '.',
  minute(tweets$time)
)


# write the data

write.table(tweets, 'data/twitter/septa_tweets.csv', 
            sep = ',', row.names = FALSE)

