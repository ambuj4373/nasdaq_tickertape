library(shiny)
library(tidyquant)
library(plotly)
library(shinythemes)

ui <- fluidPage(
  titlePanel("Stock Dashboard"),
  shinythemes::themeSelector(),
  fluidRow(
    column(width = 3,
           wellPanel(
             textInput("symbol", "Enter stock symbol (Only 4 digit USA stocks)", value = "AAPL"),
             actionButton("submit", "Submit"),
             br(),
             selectInput("example_stocks", "Select Popular Stock from Below:", 
                         c("Apple (AAPL)", 
                           "Microsoft (MSFT)", 
                           "Amazon (AMZN)", 
                           "Tesla (TSLA)", 
                           "Alphabet (GOOG)"), 
                         selected = "Apple (AAPL)"),
             actionButton("go", "Go")
           )
    ),
    column(width = 9,
           plotlyOutput("stock_plot")
    )
  )
)

server <- function(input, output, session) {
  
  submit_value <- reactiveVal(0)
  
  observeEvent(input$go, {
    stock_symbol <- strsplit(input$example_stocks, "\\(")[[1]][2]
    stock_symbol <- gsub("\\)", "", stock_symbol)
    updateTextInput(session, "symbol", value = stock_symbol)
    submit_value(submit_value() + 1)
  })
  
  stock_data <- reactive({
    if (submit_value() > 0) {
      tq_get(input$symbol, from = "2010-01-01", to = Sys.Date(), get = "stock.prices")
    }
  })
  
  output$stock_plot <- renderPlotly({
    if (!is.null(stock_data())) {
      plot_ly(stock_data(), x = ~date, y = ~close, type = "scatter", mode = "lines") %>%
        layout(title = paste("Stock Prices for", input$symbol),
               xaxis = list(title = "Date",
                            showgrid = TRUE, 
                            gridcolor = "#CDC8B1", 
                            gridwidth = 0.1),
               yaxis = list(title = "Close Price in $", 
                            showgrid = TRUE, 
                            gridcolor = "#CDC8B1", 
                            gridwidth = 0.1),
               font = list(family = "Arial", size = 12, color = "black"),
               margin = list(l = 50, r = 20, t = 50, b = 100))
    }
  })
  
}

shinyApp(ui, server)
