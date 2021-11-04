# https://www.earthdatascience.org/courses/earth-analytics/get-data-using-apis/text-mining-twitter-data-intro-r/

library(rtweet)
library(ggplot2)
library(dplyr)
#install.packages("tidytext")
library(tidytext)
library(igraph)
library(ggraph)

source("keys.R")

climate_tweets <- search_tweets(q = "#Tesla", n = 500,
                                      lang = "en",
                                      include_rts = FALSE)

# remove urls tidyverse is failing here for some reason
# climate_tweets %>%
#   mutate_at(c("stripped_text"), gsub("http.*","",.))

# remove http elements manually
climate_tweets$stripped_text <- gsub("http.*","",  climate_tweets$text)
climate_tweets$stripped_text <- gsub("https.*","", climate_tweets$stripped_text)

# remove punctuation, convert to lowercase, add id for each tweet!
climate_tweets_clean <- climate_tweets %>%
  dplyr::select(stripped_text) %>%
  tidytext::unnest_tokens(word, stripped_text)

# load list of stop words - from the tidytext package
data("stop_words")

# remove stop words from your list of words
cleaned_tweet_words <- climate_tweets_clean %>%
  anti_join(stop_words)

cleaned_tweet_words %>%
  count(word, sort = TRUE) %>%
  top_n(50) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
      labs(x = "Count",
      y = "Unique words",
      title = "Count of top 50 unique words found in #Tesla",
      subtitle = "Stop words removed from the list")