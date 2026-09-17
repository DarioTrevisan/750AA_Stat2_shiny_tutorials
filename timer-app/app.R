library(shiny)
library(ggplot2)

ui <- fluidPage(
  textOutput("rows"),
  plotOutput("plot")
)

server <- function(input, output, session) {

  # Reactive value to track the current row
  row <- reactiveVal(1)

  # Base plot (no points yet), reused whenever the plot is rebuilt or reset
  base_plot <- ggplot(iris, aes(x = Sepal.Length, y = Petal.Length))

  # Reactive value to store the current ggplot object
  current_plot <- reactiveVal(base_plot)

  output$rows <- renderText({
    paste0("Current Row: ", row())
  })

  observe({
    # Re-run this block every 1 second
    invalidateLater(1000, session)

    # isolate() so reading row()/current_plot() doesn't itself trigger a re-run
    current_row <- isolate(row())

    # Rebuild the plot from the base plot up to the current row, instead of
    # stacking a new geom_point layer on top of the previous plot each tick
    # (which would redraw all earlier points again and again).
    updated_plot <- base_plot +
      geom_point(data = iris[1:current_row, ], aes(color = Species)) +
      labs(title = paste("Current Row:", current_row))
    current_plot(updated_plot)

    # Advance to the next row, resetting once every row of iris has been shown
    if (current_row >= nrow(iris)) {
      row(1)
      current_plot(base_plot)
    } else {
      row(current_row + 1)
    }
  })

  output$plot <- renderPlot({
    current_plot()
  })
}

shinyApp(ui, server)
