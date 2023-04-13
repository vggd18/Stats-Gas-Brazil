library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("BigmacPrice.csv")

#definindo a Interface de Usuário
ui <- fluidPage(
  #titulo da pagina
  titlePanel("Analise do preço do Big Mac"),
  
  sidebarLayout(
    sidebarPanel(
        # Copy the chunk below to make a group of checkboxes
  checkboxGroupInput(
    inputId = "pais",
    label = h3("Países"), 
    choices = list("Brazil" = 1, "China" = 2),
    selected = NULL),
    hr(),
    fluidRow(column(3, verbatimTextOutput("value")))
    )
  )
  
)

server <- function(input, output) {
  
  # criando um dataframe reativo para ajudar na vizualização
  df <- reactive({
    pais %>%
      filter(????? %in% input$pais)
  })
  # You can access the values of the widget (as a vector)
  # with input$checkGroup, e.g.
  #output$value <- renderPrint({ input$checkGroup })
  
  
}

shinyApp(ui, server)