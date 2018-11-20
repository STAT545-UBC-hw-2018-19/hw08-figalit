#
# This is a Shiny web application.
#
# Author: Figali Taho
#

library(shiny)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(shinythemes)


# Load data and define necessary variables.
bcl <- read.csv("~/Desktop/STAT/hw8/BCLiquor/data/bcl-data.csv", stringsAsFactors = FALSE)
beverages <- unique(bcl$Type)


ui <- fluidPage(
  theme = shinytheme("united"),
  titlePanel("BC Liquor Price App", 
             windowTitle = "BCL app"), 
  sidebarLayout(
    sidebarPanel(
      h4(
        "Want help navigating the BC liquor store product prices? Just play around with the below filters and you'll find your answers!"
      ),
      br(),
      sliderInput("priceInput", "Select your desired price range.",
                  min = 0, max = 100, value = c(15, 30), pre="$"), 
      uiOutput("typeSelectOutput"),
      checkboxInput("filterCountry", "Filter by country", FALSE), 
      conditionalPanel(
        condition = "input.filterCountry",
        uiOutput("countrySelectorOutput")
      )
    ),
    mainPanel(
      img(src='logo.png', height = 150, width = 400, align = "center"),
      h3(textOutput("summaryText")),
      tabsetPanel(
        tabPanel("Plot", plotOutput("price_hist")), 
        tabPanel("Table", dataTableOutput("bcl_data"))
      )
    )
  )
)

server <- function(input, output) {
  output$countrySelectorOutput <- renderUI({
    selectInput("countryInput", "Country",
                sort(unique(bcl$Country)),
                selected = "CANADA")
  })
  
  output$typeSelectOutput <- renderUI({
    selectInput("typeInput", "Product type",
                sort(unique(bcl$Type)),
                multiple = TRUE,
                selected = c("BEER"))
  })
  
  bcl_filter <- reactive({
    prices <- bcl
    if (is.null(input$countryInput)) {
      return(NULL)
    }
    prices <- prices %>% 
      filter(prices$Price <= input$priceInput[2], 
             prices$Price >= input$priceInput[1], 
             Type %in% input$typeInput)
    if (input$filterCountry) {
      prices <- filter(prices, Country == input$countryInput)
    }
    if(nrow(prices) == 0) {
      return(NULL)
    }
    prices
  })
  
  output$price_hist <- renderPlot({
    if (is.null(bcl_filter())) {
      return(NULL)
    }
    
    bcl_filter() %>%
      ggplot(aes(Price, fill = Type)) +
      geom_histogram() + 
      theme_classic(20)
  })
  
  output$bcl_data <- renderTable({
    bcl_filter()
  })
  
  output$summaryText <- renderText({
    numOptions <- nrow(bcl_filter())
    if (is.null(numOptions)) {
      numOptions <- 0
    }
    paste0("We found ", numOptions, " options for you")
  })
  
  output$bcl_data <- renderDataTable({
    bcl_filter()
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
