library(shiny)

ui <- fluidPage(
    numericInput("numero", "Inserisci un numero", value=10),
    actionButton("bottone", "Aggiorna la variabile"),
    textOutput("risultato"),
    textOutput("numero_out")
)

server <- function(input, output, session) {
  x <- reactiveVal(value=input$numero)
  observeEvent(input$bottone, x(input$numero))
  output$numero_out <- renderText(paste0( "Il numero è ", input$numero) )
  output$risultato <- renderText( paste0("La reactive Val è ", x() ))
}

shinyApp(ui, server)