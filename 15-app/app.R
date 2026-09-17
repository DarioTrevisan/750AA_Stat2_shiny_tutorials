library(ggplot2)
library(shiny)

ui <- fluidPage(
  fluidRow(
    column(width = 8, class = "well",
           h4("Left plot controls right plot"),
           fluidRow(
             column(width = 6,
                    plotOutput("plot2", height = 300,
                               brush = brushOpts(
                                 id = "plot2_brush",
                                 resetOnNew = TRUE
                               ),
                               #add the hover options
                               hover = hoverOpts(
                                 id = "plot2_hover",
                                 nullOutside = TRUE
                               )
                    )
             ),
             column(width = 6,
                    #the second plot will be hidden if the user's mouse is not on the first one
                    conditionalPanel(
                      condition = "input.plot2_hover != null",
                      plotOutput("plot3", height = 300)
                    )
             )
           )
    )
    
  )
)

server <- function(input, output) {
  ranges2 <- reactiveValues(x = NULL, y = NULL)
  
  output$plot2 <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point()
  })
  
  output$plot3 <- renderPlot({
    ggplot(mtcars, aes(wt, mpg)) +
      geom_point() +
      coord_cartesian(xlim = ranges2$x, ylim = ranges2$y)
  })
  
  # When a double-click happens, check if there's a brush on the plot.
  # If so, zoom to the brush bounds; if not, reset the zoom.
  observe({
    brush <- input$plot2_brush
    print(input$plot2_hover)
    if (!is.null(brush)) {
      ranges2$x <- c(brush$xmin, brush$xmax)
      ranges2$y <- c(brush$ymin, brush$ymax)
      
    } else {
      ranges2$x <- NULL
      ranges2$y <- NULL
    }
  })
  
}

shinyApp(ui, server)