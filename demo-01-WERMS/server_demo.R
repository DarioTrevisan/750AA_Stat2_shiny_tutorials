# demo

# reactive values for the demo

# data frame to store sample points
demo_sample_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))

# minimizer position
demo_minimizer_point <- reactiveVal()

# player's chosen point (initialized at 0)
demo_player_point <- reactiveVal(0)


# reactives for the demo

# sample points and plot
demo_reactive_sample <- reactive({
  demo_sample_points(data.frame(
    x = runif(input$demo_num_points, min = -10, max = 10),
    y = rep(0, input$demo_num_points)
  ))
})

# compute the minimizer
demo_reactive_minimizer <- reactive({
  req(nrow(demo_sample_points()) > 0)
  demo_minimizer_point(empirical_minimizer(demo_sample_points()$x, input$demo_loss_choice))
})

# difference between minimizer and user's point
demo_diff <- reactive({
  req(demo_minimizer_point())
  round(abs(demo_player_point() - demo_minimizer_point()), 1)
})

# plot points

demo_reactive_plot <- reactive({
  ymax <- max(vLoss(c(-10, 10), demo_sample_points()$x, input$demo_loss_choice))
  plt <- ggplot(demo_sample_points(), aes(x = x, y = y)) +
    geom_point(
      size = {if (input$demo_num_points < 10) {4} else {2}},
      shape = 21,
      fill = "blue",
      colour = "black"
    ) +
    geom_point(
      x = demo_player_point(),
      y = 0,
      size = 6,
      shape = 3,
      color = "red"
    ) +
    lims(x = c(-10, 10), y = c(0, ymax))
  if (input$demo_show_loss) {
    plt <- plt + geom_function(
      fun = vLoss,
      args = list(points = demo_sample_points()$x, choice = input$demo_loss_choice),
      colour = "darkgrey",
      size = 2
    )
  }
  if (input$demo_show_minimizer) {
    plt <- plt + geom_point(
      x = demo_minimizer_point(),
      y = 0,
      shape = 22,
      fill = "red",
      color = "black",
      size = {if (input$demo_num_points < 10) {4} else {2}}
    )
  }
  plt
})

# general observers

observe(demo_reactive_sample())
observe(demo_reactive_minimizer())
observe(demo_reactive_plot())

# observes the player chosen point
observeEvent(input$demo_click, {
  demo_player_point(round(input$demo_click$x, 1))
})

# outputs
output$demo_plot <- renderPlot(demo_reactive_plot())
output$demo_choice_description <- renderText(paste0("Loss function: l(z)= ", loss_formula(input$demo_loss_choice)))
output$demo_proposed_minimizer <- renderText(paste0("Your choice is: ", demo_player_point()))
output$demo_minimizer_description <- renderText({
  if (input$demo_show_minimizer) {
    if (demo_diff() < 1) {
      paste0(
        "The actual minimizer is ",
        round(demo_minimizer_point(), 1),
        ", which is a distance of ",
        demo_diff(),
        " <1 from the chosen point: you WIN!"
      )
    } else {
      paste0(
        "The actual minimizer is ",
        round(demo_minimizer_point(), 1),
        ", which is a distance of ",
        demo_diff(),
        " >1 from the chosen point: try again!"
      )
    }
  } else {
    ""
  }
})
