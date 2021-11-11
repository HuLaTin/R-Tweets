library(shiny)
library(rtweet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(igraph)
library(ggraph)

source("../keys.R")

ui <- fluidPage(

    titlePanel("Search tweets"),

    # Sidebar
    sidebarLayout(
        sidebarPanel(
            numericInput("num_tweets_to_download",
                         "Number of tweets to download:",
                         min = 100,
                         max = 15000,
                         value = 500,
                         step = 100),
            textInput("hashtag_to_search",
                      "Hashtag to search:",
                      value = "#Tesla"),
            actionButton("get_data", "Get Data", class = "btn-primary")
            # br(),br(),
            # downloadButton("download_data", "Download data")
        ),

        # Show results
        mainPanel(plotOutput("plot2"))
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    hashtag_tweets <- eventReactive(input$get_data, {
        search_tweets(q = input$hashtag_to_search, n = input$num_tweets_to_download, lang = "en", include_rts = FALSE)
    })

        hashtag_tweets$stripped_text <- gsub("http.*","",  hashtag_tweets$text)
        hashtag_tweets$stripped_text <- gsub("https.*","", hashtag_tweets$stripped_text)

        hashtag_tweets_clean <- hashtag_tweets %>%
            dplyr::select(stripped_text) %>%
            tidytext::unnest_tokens(word, stripped_text)

    #if radio button for selecting to clean tweets

        data("stop_words")

        cleaned_tweet_words <- hashtag_tweets_clean %>%
            anti_join(stop_words)


    ###############################################
    output$plot2<-renderPlot({
        tweet_data$cleaned_tweets_words %>%
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
    })
}

# Run the application
shinyApp(ui = ui, server = server)
