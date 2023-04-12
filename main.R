library(shiny)
library(ggplot2)
library(dplyr)

data <- read.csv("AnaliseGas.csv")

#definindo a Interface de Usuário
ui <- fluidPage(
  #titulo da pagina
  titlePanel("Análise do preço da gasolina"),
  
  sidebarLayout(
    sidebarPanel(
      fluidPage(
        # Caixa Seletora dos estados
        selectInput("regiao", label = h3("Região"), 
                    choices = list("Norte" = 1, "Nordeste" = 2, "Centro-Oeste" = 3, "Sudeste" = 4, "Sul" = 5), 
                    selected = 1),
      )
    )
  )  
)

server <- function(input, output) {
  
  # You can access the value of the widget with input$select, e.g.
  output$value <- renderPrint({ input$regiao })
  
}

shinyApp(ui, server)