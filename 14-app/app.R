library(shiny)

ui <- fluidPage(
  actionButton("button", "genera un numero casuale!"),
  textOutput("number")
)

server <- function(input, output, session) {
  random_number <- eventReactive(input$button, runif(1))

  output$number <- renderText(paste0("Il numero casuale è :", random_number()))
}

shinyApp(ui, server)
