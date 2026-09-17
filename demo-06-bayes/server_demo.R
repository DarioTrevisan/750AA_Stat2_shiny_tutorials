# demo


demo <- reactiveValues()

# define a data frame for the data input

demo$prior_points <- data.frame(x = numeric(), y = numeric())
demo$spline <- data.frame(x = numeric(), y = numeric())
demo$likelihood <- data.frame(x = numeric(), y = numeric())
demo$posterior <- data.frame(x = numeric(), y = numeric())
demo$outcomes <- numeric()

# setup the y range for the plot

ymax <- reactive({
  if (nrow(demo$prior_points) < 1) {
    2
  }
  else{
    max(2 * max(demo$prior_points$y), 2)
  }
})

demo_mode <- reactive(if (nrow(demo$prior_points > 1))
  demo$spline$x[which.max(demo$spline$y)]
  else
    NA)

observe({
  if (input$demo_show_mode == TRUE) {
    output$demo_mode_text <- renderText(paste0(
      "The point of maximum density (mode, MAP) is ",
      round(demo_mode(), 3),
      "."
    ))
  }
  else{
    output$demo_mode_text <- renderText("")
  }
})


observeEvent(input$demo_toss_coins, {
  demo$outcomes <- rbinom(input$demo_number_runs, 1, prob = input$demo_p)
  n_ones = sum(demo$outcomes)
  n_zeros = length(demo$outcomes) - n_ones
  likelihood <- grid ** n_ones * (1 - grid) ** n_zeros
  likelihood <- likelihood / max(likelihood) * ymax()
  demo$likelihood <- data.frame(x = grid, y = likelihood)
  if (nrow(demo$spline) == 0) {
    demo$spline <- data.frame(x = grid, y = rep(1, length(grid)))
  }
  posterior <- demo$spline$y * likelihood
  posterior <- posterior / (sum(posterior) * step)
  demo$posterior <- data.frame(x = grid, y = posterior)
})


step = 0.01
grid <- seq(0, 1, by = step)


observeEvent(input$demo_bayes, {
  if (nrow(demo$prior_points) < 2) {
    game$prior_points  <- data.frame(x = c(0,1), y = c(1,1))
    demo$spline <- data.frame(x = grid, y = rep(1, length(grid)))
  }
  if (nrow( demo$likelihood) >0){
  demo$spline$y <- demo$posterior$y
  demo$likelihood <- data.frame(x = numeric(), y = numeric())
  #demo$likelihood$y <- rep(1, length(grid))
  demo$outcomes <- numeric()
  }
})


observeEvent(input$demo_normalize, {
  if (nrow(demo$prior_points) > 1) {
    z_norm <- sum(demo$spline$y) * step
    demo$spline$y <- demo$spline$y / z_norm
    demo$prior_points$y <- demo$prior_points$y / z_norm
  }
})

observe({
  if (nrow(demo$prior_points) > 1) {
    points_log <- data.frame(x = demo$prior_points$x,
                             y = log(demo$prior_points$y))
    spline_values <- c()
    for (x in grid) {
      spline_values <- c(spline_values,
                         splinefun(points_log, method = "natural")(x))
    }
    demo$spline <- data.frame(x = grid, y = exp(spline_values))
  }
})





# plot
demo_plot_reactive <- reactiveVal()



# at single click
#

observeEvent(input$demo_erase_plot, {
  demo$prior_points <- data.frame(x = numeric(), y = numeric())
  demo$spline <- data.frame(x = numeric(), y = numeric())
  demo$likelihood <- data.frame(x = numeric(), y = numeric())
  demo$posterior <- data.frame(x = numeric(), y = numeric())
  demo$outcomes <- numeric()
})

observeEvent(input$demo_remove_last_point,
             demo$prior_points <- demo$prior_points[-nrow(demo$prior_points), ])

observeEvent(input$demo_plot_click, {
  add_points <- data.frame(x = input$demo_plot_click$x,
                           y = input$demo_plot_click$y)
  demo$prior_points <- rbind(demo$prior_points, add_points)
})

observe({
  if (input$demo_show_outcomes) {
    output$demo_outcomes <- renderText(paste0(demo$outcomes, sep=";"))
  }
  else{
    output$demo_outcomes <- renderText("")
  }
})



# plot points reactive

demo_draw_points_reactive <- reactive({
  plt <- ggplot(demo$prior_points, aes(x = x, y = y)) +
    expand_limits(y = 0) +
    labs(x = "coin bias", y = "probability")+
    theme(aspect.ratio = 1)
  if (nrow (demo$prior_points) <= 2) {
    plt <- plt + lims(x = c(0, 1), y = c(0, ymax()))
  }
  if (nrow(demo$prior_points) > 1 && input$demo_show_prior == TRUE) {
    plt <- plt + geom_line(data = demo$spline,
                           aes(x = x, y = y, colour = "prior"),
                           linewidth = 2)
  }
  if (input$demo_show_points == TRUE) {
    plt <- plt +  geom_point(
      size = 2,
      shape = 16,
      colour = "black",
      fill = "black"
    )
  }
  if (input$demo_show_mode == TRUE && is.na(demo_mode()) == FALSE) {
    plt <- plt + geom_point(
      aes(x = demo_mode(), y = 0),
      color = "red",
      shape = 3,
      size = 4
    )
  }
  if (input$demo_show_likelihood == TRUE) {
    plt <- plt + geom_line(
      data = demo$likelihood,
      aes(x = x, y = y, colour = "likelihood"),
      linewidth = 2
    )
  }
  if (input$demo_show_posterior == TRUE) {
    plt <- plt + geom_line(
      data = demo$posterior,
      aes(x = x, y = y, colour = "posterior"),
      linewidth = 2
    )
  }
  plt <- plt + scale_color_manual(values = c(
    prior = "orange",
    likelihood = "blue",
    posterior = "green"
  )) + labs(colour = 'curves')
  plt
})



# # outputs
output$demo_debug <- renderPrint(demo$prior_points)
output$demo_plot <- renderPlot(demo_draw_points_reactive())