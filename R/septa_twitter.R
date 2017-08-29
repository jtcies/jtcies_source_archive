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
  tw <- sapply(accts, userTimeline, excludeReplies = TRUE, 
               since = '2016-01-01', n = 1000)
  tw.df <- twListToDF(tw)
  tab <- bind_rows(tw.df)
  tab
}

# list of septa route-specific twitter accounts

lines <- c(
  "@SEPTA_Bus", 
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
  "@SEPTA_NHSL"
)

tweets <- read_tweets(lines)

# first figure out what line it was associted with
tweets$line <- str_extract(tweets$screenName, "(?<=_).*$")

# now figure out the day of the week

tweets$date <- as.Date(tweets$created)

tweets$wday <- wday(tweets$created, label = TRUE)

# determine which tweets are associated with a delay

tweets$delay <- 0

tweets$delay[grepl('delay', tweets$text, ignore.case = TRUE)] <- 1

# plotting

tweets %>% 
  filter(delay == 1) %>% 
  ggplot(aes(x = date, fill = wday)) +
    geom_histogram(binwidth = 1) +
    scale_x_date()
