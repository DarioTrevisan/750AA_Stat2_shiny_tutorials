library(shiny)

ui <- fluidPage(
  tableOutput("static"),
  dataTableOutput("dynamic")
)
#> `shiny::dataTableOutput()` is deprecated as of shiny 1.8.1.
#> Please use `DT::DTOutput()` instead.
#> See <https://rstudio.github.io/DT/shiny.html> for more information.
server <- function(input, output, session) {
  output$static <- renderTable(head(mtcars))
  output$dynamic <- renderDataTable(mtcars, options = list(pageLength = 5))
}

shinyApp(ui, server)