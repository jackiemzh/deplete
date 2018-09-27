library(shiny)
library(xlsx)
library(markdown)
library(DT)
#library(ReporteRs)
#library(shinydashboard)
library(shinythemes)
# library(shinyjs)

library(shinycssloaders)
#


## server.R ##
shinyServer(function(input, output) {
  output$distPlot <- renderPlot({
    hist(rnorm(input$obs), col = 'darkgray', border = 'white')
  })
})
