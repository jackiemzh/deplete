
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


## ui.R ##
shinyUI(fluidPage(
  sidebarLayout(
    sidebarPanel(
      sliderInput("obs", "Number of observations:", min = 10, max = 500, value = 100)
    ),
    mainPanel(plotOutput("distPlot"))
  )
))
