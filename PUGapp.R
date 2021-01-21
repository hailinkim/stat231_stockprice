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

#make dates into a vector to prepare for plotting
date_list <- colnames(price_data)[4:953]

#set sector choices for Sector Analysis tab
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


#set app title and inputs for plot(s)
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
    
    #set tab names
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

#set plot output
server <- function(input,output){
  #data for Tab 1
  #filter data by stock selection
  use_data <- reactive({
    data <- filter(price_data, symbol %in% input$stock)
  })
  #convert to long format for plotting
  long_data <- reactive({
    use_data() %>% gather(key = "Date", value = "Price", date_list) %>%
      mutate(Date = anydate(Date))
  })
  #data for Tab3
  #filter data by sector selection
  use_data2 <- reactive({
    data <- filter(sector_data, Symbol %in% input$sector)
  })
  
  #plot for Tab 1
  output$line <- renderPlot({
    ggplot(data = long_data(), aes(x = Date, y =  Price, color = symbol)) +
             geom_line() +
              xlab("Date") +
              ylab("Price ($)") +
              labs(color = "Company Name") +
              ggtitle("Share Price Performance") +
              theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
                    axis.title.y = element_text(size = 14),
                    axis.title.x = element_text(size = 14),
                    axis.text.x = element_text(size = 12),
                    legend.text = element_text(size = 10))
  })
  
  #plot for Tab2
  output$table <- renderTable({
    dplyr::select(use_data(), state, input$x, input$y)
  })
  
  #plot for Tab3
  output$sector <- renderPlot({
    ggplot(data = use_data2(), aes(x = as.Date(Date), y =  Open, color = as.factor(Symbol))) +
      geom_line() +
      xlab("Date") +
      ylab("Price ($)") +
      ggtitle('Sector Index Price Performance') +
      theme(plot.title = element_text(size = 16, face = "bold", hjust = 0.5),
            axis.title.y = element_text(size = 14),
            axis.title.x = element_text(size = 14),
            axis.text.x = element_text(size = 12),
            legend.text = element_text(size = 10))
  })
}

# call to shinyApp
shinyApp(ui = ui, server = server)
