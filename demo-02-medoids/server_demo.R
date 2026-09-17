# demo

# reactive values for the demo

demo <- reactiveValues()

# data frame to store sample points
demo$sample_points <- data.frame(x = numeric(), y = numeric())

# actual medoid index
demo_medoid_point <- reactive(compute_medoid(demo$sample_points))

# player's chosen point index
demo_player_point <- reactiveVal()

# plot
demo_plot_reactive <- reactiveVal()



# observe events for the demo

# at single click propose medoid or draw points

observeEvent(input$demo_plot_click, 
             {if(input$demo_radio == "demo_select"){
  demo_player_point(closest_point(
    data.frame(
      x = input$demo_plot_click$x,
      y = input$demo_plot_click$y
    ),
    demo$sample_points
  ))
  demo_plot_reactive(
    demo_draw_points_reactive() + geom_point(
      aes(x = x, y = y),
      data = demo$sample_points[demo_player_point(), ],
      colour = "black",
      size = 7,
      shape = 21,
      fill = "white",
      alpha = 0.8
    )
  )}
               else{
                 add_row <- data.frame(x = input$demo_plot_click$x,
                                       y = input$demo_plot_click$y)
                 # add row to the sample points
                 demo$sample_points <- rbind(demo$sample_points, add_row)
                 demo_plot_reactive(demo_draw_points_reactive())
               }
})


# button actions

observeEvent(input$demo_remove_medoid, {
  demo$sample_points <- demo$sample_points[-demo_medoid_point(), ]
  demo_plot_reactive(demo_draw_points_reactive())
})

observeEvent(input$demo_erase_plot, {
  demo$sample_points <- data.frame(x = numeric(), y = numeric())
  demo_plot_reactive(demo_draw_points_reactive())
})

observeEvent(input$demo_show_medoid, {
  plt <- demo_draw_points_reactive()
  plt <- plt +
    geom_point(
      aes(x = x, y = y),
      data = demo$sample_points[demo_player_point(), ],
      colour = "black",
      size = 7,
      shape = 21,
      fill = "white",
      alpha = 0.8
    ) +
    geom_point(
      aes(x = x, y = y),
      data = demo$sample_points[demo_medoid_point(), ],
      colour = "red",
      size = 10,
      shape = 10
    )
  demo_plot_reactive(plt)
  if (demo_medoid_point() == demo_player_point())
  {
    output$demo_info <- renderText(paste0("Your guess is right!"))
  }
  else {
    output$demo_info <- renderText(paste0("Your guess is wrong!"))
  }
})


# plot points reactive

demo_draw_points_reactive <- reactive({
  ggplot(demo$sample_points, aes(x = x, y = y)) +
    geom_point(
      size = 5,
      shape = 22,
      colour = "blue",
      fill = "lightblue"
    ) +
    lims(x = c(0, 1), y = c(0, 1)) +
    theme(aspect.ratio = 1)
})



# general observers

observe(demo_medoid_point())


# outputs
output$demo_plot <- renderPlot(demo_plot_reactive())



