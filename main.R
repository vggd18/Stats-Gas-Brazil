library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("BigmacPrice.csv")

# Define UI
ui <- fluidPage(
  # TITULO DO APP
  titlePanel("Análise do preço dos Big Mac's"),
  # Barra lateral com os inputs
  sidebarLayout(
    # painel da Barra Lateral
    sidebarPanel(
      # Input de Seleção de quais produtos Analisar
      selectInput(inputId = "name", 
                  label = "Choose a Country", 
                  choices = unique(data$name)), 
      plotOutput("line"),
      
      checkboxGroupInput(
        inputId = "main",
        label = "Select Currency", 
        choices = c("Brazil", "USA", "China"),
        selected = NULL
      ),
      # Input com as datas iniciais e finais para Análise
      dateInput("start_date", "Data Inicial:", value = min(data$date)),
      dateInput("end_date", "Data Final:", value = max(data$date)),
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
      filter(date >= input$start_date & date <= input$end_date) %>%
      filter(name %in% input$main)
  })
  
  plot_line_chart <- function(data) {
    ggplot(data, aes(x = date, y = !!sym("dollar_price"), color = name, group = name)) +
      geom_point() +
      geom_line() +
      labs(x = "Data", y = "preco do Bigmac") +
      theme_minimal()
  }
  
  output$line_chart <- renderPlot({
    plot_line_chart(filtered_data())
  })
  
  output$histogram <- renderPlot({
    ggplot(data %>% filter(name %in% input$name), aes(x = !!sym("dollar_price"), color = name, group = name)) +
      geom_histogram(bins = 30) +
      labs(x = "Preço da moeda", y = "Frequência") +
      theme_minimal()
  })
  
  
  observeEvent(input$update_button, {
    output$line_chart <- renderPlot({
      plot_line_chart(filtered_data())
    })
    output$histogram <- renderPlot({
      ggplot(data %>% filter(name %in% input$name), aes(x = !!sym("dollar_price"), color = name, group = name)) +
        geom_histogram(bins = 30) +
        labs(x = "Preço da moeda", y = "Frequência") +
        theme_minimal()
    })
  })
  
}

shinyApp(ui, server)