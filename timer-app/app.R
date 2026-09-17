library(shiny)
library(ggplot2)


ui <- fluidPage(
  textOutput("rows"),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  output$rows <- renderText({
    paste0("Current Row: ", row())
  })
  
  # Reactive value to track the current row
  row <- reactiveVal(1)
  
  # Reactive value to store the ggplot object
  plot <- reactiveVal()
  
  plot( ggplot(iris, aes(x = Sepal.Length, y = Petal.Length))) 
  
   observe({
  #   # Update the plot every 1 second
     invalidateLater(1000, session)
  #   
  #   # Use isolate for current_row and plot updates
     current_row <- isolate(row())
  #   
  #   # Update the plot with a new point
     updated_plot <- isolate(plot()) + 
      geom_point(data = iris[1:current_row, ], aes(color = Species)) +
       labs(title = paste("Current Row:", current_row))
  #   
     plot(updated_plot)
  #   
  #   # Increment the row
     row(current_row + 1)
  #   
  #   # Reset after reaching the number of rows in iris
     if (current_row >= 4) {
       row(1)
       plot(ggplot(iris, aes(x = Sepal.Length, y = Petal.Length)))  # Reset plot
     }
  })
  
  output$plot <- renderPlot({
    plot()
  })
}

shinyApp(ui, server)