library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("AnaliseGas.csv")

# Define UI
ui <- fluidPage(
  # TITULO DO APP
  titlePanel("Análise do preço da gasolina"),
  # Barra lateral com os inputs
  sidebarLayout(
    # painel da Barra Lateral
    sidebarPanel(
      # Input de Seleção de estados para Análise
      selectInput(
        inputId = "y",
        label = "Estados",
        choices = c("Norte", "Nordeste", "Sul", "Sudeste", "Centro-Oeste"),
        selected = "Norte"
      ),
      # Input de Seleção de quais produtos Analisar
      checkboxGroupInput(
        inputId = "PRODUTO",
        label = "Produto", 
        choices = c("Etanol Hidratado", "Gasolina Comum", "GLP", "GNV", "Óleo Diesel", "Óleo Diesel S10"),
        selected = c("Gasolina Comum", "GLP")
      ),
      # Input com as datas iniciais e finais para Análise
      dateInput("start_date", "Data Inicial:", value = min(data$DATA_INICIAL)),
      dateInput("end_date", "Data Final:", value = max(data$DATA_INICIAL)),
      # Botão para atualizar os gráficos
      actionButton("update_button", "Atualizar")
    ),
    
    #painel principal
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      #tabsetPanel(type = "tabs",
                  #tabPanel("Linha", plotOutput("line_chart")),
                  #tabPanel("Histograma", plotOutput("histogram"))
                  
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
      filter(DATA_INICIAL >= input$DATA_INICIAL & DATA_INICIAL <= input$DATA_INICIAL) %>%
      filter(PRODUTO %in% input$PRODUTO)
    
  })
  
  plot_line_chart <- function(data) {
    ggplot(data, aes(x = DATA_INICIAL, y = !!sym(input$y))) +
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