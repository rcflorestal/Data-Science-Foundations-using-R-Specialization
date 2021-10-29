library(shiny)


# Define UI for application
shinyUI(fluidPage(
        
        # Application title
        titlePanel('Word Predictor'),
        

        # Sidebar with a text input
        sidebarLayout(
                      fluidRow(
                      ),
                      

                      # Show a panel text
                      mainPanel(
                              textAreaInput('text', 
                                            label = 'Type a word and wait', 
                                            width = '100%', cols = 120, 
                                            rows = 3, placeholder = NULL, 
                                            resize = NULL),
                              tableOutput('nextWord')
                              
                      )
        ),
        # Notes
        tags$h3('Note'),
        tags$article('This output comes from the most frequent trigrams extracts from US Twitter, US blogs, and Us news, using natural language processing through R language.'),
        tags$h4('Read'),
        tags$a(href='https://github.com/rcflorestal/Data-Science-Specialization/tree/main/Data-Science-Capstone/Peer-Graded', 
               'Read about it in the github repository'),
))
