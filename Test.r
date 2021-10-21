# If you need to install any of these:
install.packages("rtweet")
install.packages("reactable")
install.packages("glue")
install.packages("stringr")
install.packages("httpuv")
install.packages("dplyr")
install.packages("purrr")
library(rtweet)
library(dplyr)

tweet_df <- search_tweets("#tesla", n = 200, 
            include_rts = FALSE)

tweet_table_data <- select(tweets, -user_id, -status_id)
library(reactable)
reactable(tweet_table_data)

reactable(tweet_table_data, 
          filterable = TRUE, searchable = TRUE, bordered = TRUE, 
          striped = TRUE, highlight = TRUE,
          defaultPageSize = 25, showPageSizeOptions = TRUE, 
          showSortable = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), defaultSortOrder = "desc",
            columns = list(
            created_at = colDef(defaultSortOrder = "asc"),
            screen_name = colDef(defaultSortOrder = "asc"),
            text = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
            favorite_count = colDef(filterable = FALSE),
            retweet_count = colDef(filterable =  FALSE),
            urls_expanded_url = colDef(html = TRUE)
          )
) 

glue::glue("https://twitter.com/{screen_name}/status/{status_id}")

Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 

tweet_table_data <- tweet_df %>%
  select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
  mutate(
    Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>") 
    )%>%
  select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs = urls_expanded_url)

make_url_html <- function(url) {
  if(length(url) < 2) {
    if(!is.na(url)) {
      as.character(glue("<a title = {url} target = '_new' href = '{url}'>{url}</a>") )
    } else {
      ""
    }
  } else {
    paste0(purrr::map_chr(url, ~ paste0("<a title = '", .x, "' target = '_new' href = '", .x, "'>", .x, "</a>", collapse = ", ")), collapse = ", ")
  }
}

tweet_table_data$URLs <- purrr::map_chr(tweet_table_data$URLs, make_url_html)

reactable(tweet_table_data, 
          filterable = TRUE, searchable = TRUE, bordered = TRUE, striped = TRUE, highlight = TRUE,
          showSortable = TRUE, defaultSortOrder = "desc", defaultPageSize = 25, showPageSizeOptions = TRUE, pageSizeOptions = c(25, 50, 75, 100, 200), 
          columns = list(
            DateTime = colDef(defaultSortOrder = "asc"),
            User = colDef(defaultSortOrder = "asc"),
            Tweet = colDef(html = TRUE, minWidth = 190, resizable = TRUE),
            Likes = colDef(filterable = FALSE, format = colFormat(separators = TRUE)),
            RTs = colDef(filterable =  FALSE, format = colFormat(separators = TRUE)),
            URLs = colDef(html = TRUE)
          )
) 