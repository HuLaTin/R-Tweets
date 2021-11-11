# https://www.infoworld.com/article/3516150/create-a-shiny-app-to-search-twitter-with-rtweet-and-r.html

library(shiny)
library(rtweet)
library(dplyr)
library(glue)
library(reactable)
library(purrr)

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

ui <- fluidPage(
  
  # Application title
  titlePanel("Search tweets"),
  
  # Sidebar 
  sidebarLayout(
    sidebarPanel(
            numericInput("num_tweets_to_download",
                         "Number of tweets to download:",
                         min = 100,
                         max = 15000,
                         value = 100,
                         step = 100),
            textInput("hashtag_to_search",
                      "Hashtag to search:",
                      value = "#Tesla"),
            actionButton("get_data", "Get data", class = "btn-primary"),
            br(),br(),
            downloadButton("download_data", "Download data")
        ),
    
    # Show results
    mainPanel(
      reactableOutput("tweet_table")
    )
  )
)

# Define server logic 
server <- function(input, output) {
  
  tweet_df <- eventReactive(input$get_data, {
    search_tweets(input$hashtag_to_search, n = input$num_tweets_to_download, include_rts = FALSE)
    })
  
  tweet_table_data <- reactive({
    req(tweet_df())
    tweet_df() %>%
      select(user_id, status_id, created_at, screen_name, text, favorite_count, retweet_count, urls_expanded_url) %>%
      mutate(
        Tweet = glue::glue("{text} <a href='https://twitter.com/{screen_name}/status/{status_id}'>>> </a>"),
        URLs = purrr::map_chr(urls_expanded_url, make_url_html)
      )%>%
      select(DateTime = created_at, User = screen_name, Tweet, Likes = favorite_count, RTs = retweet_count, URLs)
  })
  
  output$tweet_table <- renderReactable({
    reactable::reactable(tweet_table_data(), 
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
  })

  output$download_data <- downloadHandler(
    filename = function() {
      paste(input$hashtag_to_search, "_", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      write.csv(tweet_table_data(), file, row.names = FALSE)
    }
  )
  
}

# Run the application 
shinyApp(ui = ui, server = server)