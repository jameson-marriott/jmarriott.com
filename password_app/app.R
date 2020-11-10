#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)

titles <- gutenberg_works(only_text = TRUE) %>%
    select(title) %>%
    drop_na()

data("stop_words")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("XKCD-Inspired, Gutenberg-Sourced Passwords"),

    # Sidebar with a slider input for number of words
    sidebarLayout(
        sidebarPanel(
            selectizeInput("book_title", 
                           "Book Title",
                           titles,
                           "Pride and Prejudice"),
            sliderInput("number_of_words",
                        "Number of words",
                        min = 1,
                        max = 10,
                        value = 4)
        ),

        # Show the password
        mainPanel(
           textOutput("password")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    gutenberg_book <- reactive({
        gutenberg_works(title == input$book_title) %>% # get the gutenberg id
            gutenberg_download() %>% 
            unnest_tokens(word, text) %>% # turn the text into a single column of words
            mutate(word = str_extract(string = word, pattern = "[[:alpha:]]+")) %>% # remove any non-alphanumeric characters. 
            select(word) %>% # get rid of the extra columns
            unique() %>% # get rid of duplicate words
            anti_join(stop_words, by = "word") %>% # get rid of boring, "stop" words
            drop_na() %>% # drop anything that didn't make it through cleanly
            unlist()
    })
    # re-downloads the book every time a new number of words in selected - should not do that. 
    output$password <- renderText({
        gutenberg_book() %>%
            sample(input$number_of_words) %>% # chose four words at random
            paste0() # drop the names
    })
}

# Run the application 
shinyApp(ui = ui, server = server)










