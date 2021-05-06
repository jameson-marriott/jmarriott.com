library(shiny)
library(shinythemes)
library(gutenbergr)
library(dplyr)
library(tidyr)
library(tidytext)
library(stringr)
library(rclipboard)

# get all the titles for the drop-down menu
# use the next commented lines to download the titles one time and save them, 
# but then use the last un-commented line to load them faster.
# titles <- gutenberg_works(only_text = TRUE, distinct = TRUE) %>%
#      select(title) %>%
#      drop_na()
# write_csv(titles, "titles.csv")
titles <- read.csv("titles.csv") # improvement: add authors to title

# load the stop words so that we don't have to reload it later
data("stop_words")

ui <- fluidPage(theme = shinytheme("cerulean"),
                
                rclipboardSetup(), # what it sounds like
                
    verticalLayout(
        fluidRow(
            column(width = 8, offset = 1,
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
                   selectizeInput(inputId = "book_title", 
                                  label = "Book Title",
                                  choices = NULL), # uses server-side selectize
                                  #selected = NULL),
                                  #choices = titles,
                                  #selected = "Pride and Prejudice"),
                   p(textOutput("book_length")),
                   sliderInput("number_of_words",
                               "Number of words to chose",
                               min = 1,
                               max = 10,
                               value = 4))
        ),

        # show the password
        fluidRow(
            column(width = 6, offset = 1,
                   tags$hr(),
                   textOutput("password", container = tags$strong)
            ),
        ),
        # show the password without spaces
        fluidRow(
            column(width = 6, offset = 1,
                   uiOutput("password_no_spaces"))
        )
    )
)

server <- function(input, output, session) {
    
    # server-side selectize
    updateSelectizeInput(session, 
                         inputId = "book_title", 
                         choices = titles$title, 
                         selected = sample(titles$title, 1), # would be better to initially select a book with more than 0 words
                         server = TRUE)
    
    # get the book
    gutenberg_book <- reactive({
        validate(
            need(input$book_title != "", "Please chose a book.")
        )
        gutenberg_works(title == input$book_title) %>% # get the gutenberg id
            gutenberg_download(mirror = "http://mirrors.xmission.com/gutenberg/") %>% 
            unnest_tokens(word, text) %>% # turn the text into a single column of words
            mutate(word = str_extract(string = word, pattern = "[[:alpha:]]+")) %>% # remove any non-alphanumeric characters. 
            select(word) %>% # get rid of the extra columns
            unique() %>% # get rid of duplicate words
            anti_join(stop_words, by = "word") %>% # get rid of boring, "stop" words
            drop_na() %>% # drop anything that didn't make it through cleanly
            unlist()
    })
    
    # report the number of unique words in the book
    output$book_length <- renderText({
        length <- gutenberg_book() %>%
            length() %>%
            format(big.mark = ",") # add some nice formatting
        
        paste0("There are ", length, " unique words in this book (including diffent forms of the same word).")
    })
     
    # generate the actual password from the book
    password <- reactive({
        # validate(
        #     need(input$book_title != "", "")
        # )
        validate(
            need(length(gutenberg_book()) > 0, message = "This book didn't load properly. Please try another one.")
        )
        gutenberg_book() %>%
            sample(input$number_of_words) %>% # chose words at random
            paste0() # drop the names
    })
    
    # output the password for the UI
    output$password <- renderText({
        password()
    })
    
    # make the button to copy the password to the clipboard
    output$password_no_spaces <- renderUI({
        rclipButton("clip_button", paste0("Copy \"", str_flatten(password()), "\""), str_flatten(password()))
    })
}

# run the application 
shinyApp(ui = ui, server = server)
