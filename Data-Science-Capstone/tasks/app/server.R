library(shiny)
library(sbo)

p <- readRDS('./listApp.RDS')
p <- sbo_predictor(p)

# Define server
shinyServer(function(input, output, session) {
        
        
        input_txt <- reactive({
                param <- input$text
                param
        })
        
        
        output$nextWord <- renderTable({
                sent <- input_txt()
                x <- predict(p, sent)
                df <- data.frame(as.integer(c(1, 2, 3)))
                df[,2] <- c(x[1], x[2], x[3])
                names(df) <- c('Rank', 'Predicted Word')
                df
        })
        session$onSessionEnded(function() {
                stopApp()
        })
})
