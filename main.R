library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("crypto-markets.csv")

ui <- fluidPage(
  titlePanel("Dashboard do CSV Crypto Markets"),
  sidebarLayout(
    sidebarPanel(
      selectInput(
        inputId = "y",
        label = "eixo-y",
        choices = c("volume", "close"),
        selected = "close"
      ),
      checkboxGroupInput(
        inputId = "name",
        label = "Select Currency", 
        choices = c("Bitcoin", "Ethereum", "Litecoin"),
        selected = c("Bitcoin", "Ethereum", "Litecoin")
      ),
      dateInput("start_date", "Data Inicial:", value = min(data$date)),
      dateInput("end_date", "Data Final:", value = max(data$date)),
      actionButton("update_button", "Atualizar")
    ),
    mainPanel(
      tags$h3("Gráfico de Linha"),
      plotOutput("line_chart"),
      tags$h3("Histograma"),
      plotOutput("histogram")
    )
  )
)

server <- function(input, output){
  
  filtered_data <- reactive({
    data %>%
      filter(date >= input$start_date & date <= input$end_date) %>%
      filter(name %in% input$name)
    
  })
  
  plot_line_chart <- function(data) {
    ggplot(data, aes(x = date, y = !!sym(input$y))) +
      geom_line() +
      labs(x = "Data", y = "Preço da moeda") +
      theme_minimal()
  }
  
  output$line_chart <- renderPlot({
    plot_line_chart(filtered_data())
  })
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = !!sym(input$y))) +
      geom_histogram(bins = 30) +
      labs(x = "Preço da moeda", y = "Frequência") +
      theme_minimal()
  })
  
  observeEvent(input$update_button, {
    output$line_chart <- renderPlot({
      plot_line_chart(filtered_data())
    })
    output$histogram <- renderPlot({
      ggplot(filtered_data(), aes(x = !!sym(input$y))) +
        geom_histogram(bins = 30) +
        labs(x = "Preço da moeda", y = "Frequência") +
        theme_minimal()
    })
  })
  
}

shinyApp(ui, server)