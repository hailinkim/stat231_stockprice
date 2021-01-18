#load necessary libraries
library(quantmod)
library(shinythemes)
library(tidyverse)
library(janitor)
library(anytime)

#read in data, remove X's from date columns
price_data <- read.csv('/Users/angelica/git/Shiny-Stratton-Oakmont/final.csv') %>%
  janitor::clean_names()
names(price_data)[4:953] <- substring(names(price_data)[4:953], 2, 11)

sector_data <- read.csv('/Users/angelica/git/Shiny-Stratton-Oakmont/sector_data.csv')

#make dates a list
date_list <- colnames(price_data)[4:953]


sector_choices <- as.list(c( unique(sector_data$Symbol)))
sector_choices_names <- c("Healthcare"
                          , "Industrials"
                          , "Consumer Staples"
                          , "Consumer Discretionary"
                          , "Materials"
                          , "Utilities"
                          , "Financials"
                          , "Energy"
                          , "Technology"
                          , "Real Estate"
                          , "Communication Services")
names(sector_choices) <- sector_choices_names


#set inputs for plot(s)
ui <- fluidPage(
  
  h1("Stock Price Data 2017-Present"),
  
  sidebarLayout(
    sidebarPanel(
      textInput(inputId = "stock"
                , label = "Choose Stock Ticker(s)"
                , value = ""),
      selectInput(inputId = "sector"
                  , label = "Choose S&P Sector"
                  , choices = sector_choices
                  , selected = "")
    ),
    
    mainPanel(
      
      tabsetPanel(type = "tabs"
                  , tabPanel("Plot of Prices"
                             , plotOutput(outputId = "line"))
                  , tabPanel("Bar Plot of PE Ratios", plotOutput(outputId = "bar"))
                  , tabPanel("Sector Analysis", plotOutput(outputId = "sector"))
      )
    )
  )
)

server <- function(input,output){
  
  use_data <- reactive({
    data <- filter(price_data, symbol %in% input$stock)
  })
  long_data <- reactive({
    use_data() %>% gather(key = "Date", value = "Price", date_list) %>%
      mutate(Date = anydate(Date))
  })
  
  use_data2 <- reactive({
    data <- filter(sector_data, Symbol %in% input$sector)
  })
  
  output$line <- renderPlot({
    ggplot(data = long_data(), aes(x = Date, y =  Price, color = symbol)) +
             geom_line() 
      })
  
  output$sector <- renderPlot({
    ggplot(data = use_data2(), aes(x = as.Date(Date), y =  Open, color = as.factor(Symbol))) +
      geom_line() +
      xlab("Date") +
      ylab("Sector Index") +
      theme(legend.position = "none")
  })

  
  
  output$table <- renderTable({
    dplyr::select(use_data(), state, input$x, input$y)
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)
