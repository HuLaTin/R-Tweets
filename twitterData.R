# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/use-twitter-api-r/
# https://www.rdocumentation.org/packages/rtweet/versions/0.7.0/topics/search_tweets
# https://cran.r-project.org/web/packages/rtweet/vignettes/intro.html

library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)

#keys stored for security
source("keys.R")

#post a tweet
#post_tweet("This Tweet sent using R programming! https://github.com/HuLaTin/R-Tweets")

rstats_tweets <- search_tweets("#Tesla", n = 500,
                             include_rts = FALSE, geocode = NULL)

#head(rstats_tweets, n = 3)

#unique(rstats_tweets$screen_name)

users <- search_users("#Tesla",
                      n = 500)

#head(users, n = 2)

# how many locations are represented
length(unique(users$location))

users %>%
  count(location, sort = TRUE) %>%
  mutate(location = reorder(location, n)) %>%
  top_n(20) %>%
  ggplot(aes(x = location, y = n)) +
  geom_col() +
  coord_flip() +
      labs(x = "Count",
      y = "Location",
      title = "Where Twitter users are from - unique locations ")