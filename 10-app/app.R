library(shiny)

ui <- fluidPage(
  checkboxInput("somevalue", "Some value", FALSE),
  verbatimTextOutput("value")
)

server <- function(input, output) {
  output$value <- renderText({
    if (input$somevalue) {
      "Checkbox is checked!"
    } else {
      ""  # Return empty string when checkbox is not checked
    }
  })
}

shinyApp(ui, server)