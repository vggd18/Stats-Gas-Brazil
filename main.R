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
      # Input de Seleção de regioes para Análise
      selectInput(
        inputId = "REGIÃO",
        label = "REGIOES",
        choices = c("NORTE", "NORDESTE", "SUL", "SUDESTE", "CENTRO OESTE"),
        selected = "NORTE"
      ),
      # Input de Seleção de quais produtos Analisar
      checkboxGroupInput(
        inputId = "PRODUTO",
        label = "Produto", 
        choices = c("ETANOL HIDRATADO", "GASOLINA COMUM", "GLP", "GNV", "ÓLEO DIESEL", "ÓLEO DIESEL S10"),
        selected = c("GASOLINA COMUM", "GLP")
      ),
      # Input com as datas iniciais e finais para Análise
      dateInput("start_date", "Data Inicial:", value = min(data$DATE)),
      dateInput("end_date", "Data Final:", value = max(data$DATE)),
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
      filter(DATE >= input$start_date & DATE <= input$end_date) %>%
      filter(PRODUTO %in% input$PRODUTO) %>%
      filter(REGIÃO %in% input$REGIÃO)
  })
  
  plot_line_chart <- function(data) {
    ggplot(data, aes(x = DATE, y = "PREÇO MÉDIO")) +
      geom_line() +
      labs(x = "Data", y = "Preço da moeda") +
      theme_minimal()
  }
  
  output$line_chart <- renderPlot({
    plot_line_chart(filtered_data())
  })
  
  output$histogram <- renderPlot({
    ggplot(filtered_data(), aes(x = "PREÇO MÉDIO")) +
      geom_histogram(bins = 30) +
      labs(x = "Preço da moeda", y = "Frequência") +
      theme_minimal()
  })
  
  observeEvent(input$update_button, {
    output$line_chart <- renderPlot({
      plot_line_chart(filtered_data())
    })
    output$histogram <- renderPlot({
      ggplot(filtered_data(), aes(x = "PREÇO MÉDIO")) +
        geom_histogram(bins = 30) +
        labs(x = "Preço da moeda", y = "Frequência") +
        theme_minimal()
    })
  })
  
}

shinyApp(ui, server)