library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("BigmacPrice.csv")

# criando um sumário de cada espécie e salvando em daframes separados
Tabela_a <- data %>%
  group_by(name) %>%
  summarise(Moda = as.numeric(names(table(dollar_price)[table(dollar_price) == max(table(dollar_price))])),
            Média = median(dollar_price),
            Mediana = mean(dollar_price),
            `Desvio padrão` = sd(dollar_price))

# Define UI
ui <- fluidPage(
  # TITULO DO APP
  titlePanel("Análise do preço dos Big Mac's"),
  # Barra lateral com os inputs
  sidebarLayout(
    # painel da Barra Lateral
    sidebarPanel(
      #checkbox
      checkboxGroupInput(
        inputId = "main",
        label = "Select Currency", 
        choices = c("Brazil", "USA", "China"),
        selected = NULL
      ),
      
      # Input com as datas iniciais e finais para Análise
      dateInput("start_date", "Data Inicial:", value = min(data$date)),
      dateInput("end_date", "Data Final:", value = max(data$date)),
      
      selectInput(inputId = "name", 
                  label = "Choose a Country", 
                  choices = unique(data$name)), 
      plotOutput("line"),
      
      # Botão para atualizar os gráficos
      actionButton("update_button", "Atualizar")
    ),
    
    #painel principal
    mainPanel(
      # Output: Tabset w/ plot, summary, and table ----
      tabsetPanel(type = "tabs",
      tabPanel("Gráfico de Linha", plotOutput("line_chart")),
      tabPanel("Histograma", plotOutput("histogram")),
      tabPanel("Boxplot", plotOutput("boxplot")),
      tabPanel("Tabela de Dados", tableOutput("table"))
      )
  )
)
)
server <- function(input, output){
  filtered_data <- reactive({
    data %>%
      filter(date >= input$start_date & date <= input$end_date) %>%
      filter(name %in% input$main)
  })
  # tabela dos dados sumarizados
  output$table <- renderTable(Tabela_a)
  
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
  
  output$boxplot <- renderPlot({
    ggplot(filtered_data(), aes(x = name, y = dollar_price)) +
      geom_boxplot(fill="slateblue", alpha=0.2) + 
      xlab("País") +
      ylab("Preço do Big Mac")
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
    output$boxplot <- renderPlot({
      ggplot(filtered_data(), aes(x = name, y = dollar_price)) +
        geom_boxplot(fill="slateblue", alpha=0.2) + 
        xlab("País") +
        ylab("Preço do Big Mac")
    })

  })
  
}

shinyApp(ui, server)