#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(shinythemes)
library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(rclipboard)

titles <- gutenberg_works(only_text = TRUE) %>%
    select(title) %>%
    drop_na()

data("stop_words")

# Define UI for application that draws a histogram
ui <- fluidPage(theme = shinytheme("cerulean"),
                
                rclipboardSetup(),
                
    verticalLayout(
        fluidRow(
            column(width = 8, offset = 1,
                   # Application title
                   titlePanel(title = "XKCD-Inspired, Gutenberg-Sourced Passwords"),
                   p("This web-app lets you generate passwords inspired by ",
                      a(href = "https://xkcd.com/936/", "this xkcd comic."),
                      br(),
                      "First select a book from ",
                      a(href = "https://www.gutenberg.org/", "Project Gutenberg"),
                      " and then chose the number of words you want to use from that book for your password.")
                   ),
        ),
        fluidRow(
            column(width = 6, offset = 1,
                   selectizeInput("book_title", 
                                  "Book Title",
                                  #c("Chose one" = "", titles)), # removes the default selection, but needs error handling for the down-stream items
                                  titles,
                                  "Pride and Prejudice"),
                   p(textOutput("book_length")),
                   sliderInput("number_of_words",
                               "Number of words to chose",
                               min = 1,
                               max = 10,
                               value = 4))
        ),

        # Show the password
        fluidRow(
            column(width = 6, offset = 1,
                   tags$hr(),
                   textOutput("password", container = tags$strong)
            ),
        ),
        fluidRow(
            column(width = 6, offset = 1,
                   uiOutput("password_no_spaces"))
        )
    )
)

# Define server logic
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
    
    # This doesn't work for some reason. 
    output$book_length <- renderText({
        length <- gutenberg_book() %>%
            length() %>%
            as.character()
        paste0("Number of unique words in this book: ", length, ".")
    })
     
    password <- reactive({
        gutenberg_book() %>%
            sample(input$number_of_words) %>% # chose four words at random
            paste0() # drop the names
    })
    
    output$password <- renderText({
        password()
    })
    
    output$password_no_spaces <- renderUI({
        rclipButton("clip_button", paste0("Copy \"", str_flatten(password()), "\""), str_flatten(password()))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)










