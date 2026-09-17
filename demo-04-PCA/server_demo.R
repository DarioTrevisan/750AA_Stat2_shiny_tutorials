# demo

# demo status meanings:
#   -1 expects the player to click to draw data points
#    0 expects the player to click to place the first point of a line
#    1 expects the player to click to place the second point of a line
#    2 shows the resulting line and the variance comparison

demo_status <- reactiveVal(-1)

demo <- reactiveValues(data = data.frame(x = numeric(), y = numeric()))

# store the point coordinates submitted by the player
demo_player <- reactiveValues(points = data.frame(x = numeric(), y = numeric()))

# switch between "draw points" and "draw line" input modes
observe({
  if (input$radio == -1) {
    demo_status(-1)
  } else if (input$radio == 0) {
    demo_status(0)
  }
})

observeEvent(input$demo_erase_plot, {
  demo$data <- data.frame(x = numeric(), y = numeric())
  demo_player$points <- data.frame(x = numeric(), y = numeric())
  demo_status(-1)
})

observeEvent(input$demo_plot_click, {
  click <- data.frame(x = input$demo_plot_click$x, y = input$demo_plot_click$y)
  if (demo_status() == -1) {
    demo$data <- rbind(demo$data, click)
    demo_player$points <- data.frame(x = numeric(), y = numeric())
  } else if (demo_status() == 0) {
    demo_player$points <- click
    demo_status(1)
  } else if (demo_status() == 1) {
    demo_player$points <- rbind(demo_player$points, click)
    demo_status(2)
  } else {
    demo_player$points <- data.frame(x = numeric(), y = numeric())
    demo_status(0)
  }
})

# derived reactives -----------------------------------------------------

# covariance matrix and first principal component of the drawn points
demo_data_cov <- reactive({
  req(nrow(demo$data) >= 2)
  cov(demo$data)
})

demo_data_pc1 <- reactive({
  req(nrow(demo$data) >= 2)
  prcomp(demo$data)$rotation[, 1]
})

demo_pc1_slope <- reactive(as.numeric(demo_data_pc1()[2] / demo_data_pc1()[1]))
demo_pc1_intercept <- reactive(mean(demo$data$y) - demo_pc1_slope() * mean(demo$data$x))

# player's proposed direction, once both line points are placed
demo_player_direction <- reactive({
  req(nrow(demo_player$points) == 2)
  v <- as.numeric(demo_player$points[2, ] - demo_player$points[1, ])
  v / sqrt(sum(v ^ 2))
})

demo_player_slope <- reactive({
  v <- demo_player_direction()
  as.numeric(v[2] / v[1])
})

demo_player_intercept <- reactive({
  point1 <- demo_player$points[1, ]
  as.numeric(point1$y - demo_player_slope() * point1$x)
})

# variance along the player's direction and along the true PC1 direction
demo_sigma_player <- reactive({
  v <- demo_player_direction()
  sqrt(sum(v * (demo_data_cov() %*% v)))
})

demo_sigma_pc <- reactive({
  pc <- demo_data_pc1()
  sqrt(sum(pc * (demo_data_cov() %*% pc)))
})

# plot --------------------------------------------------------------------

demo_draw_points_reactive <- reactive({
  plt <- ggplot(demo$data, aes(x = x, y = y)) +
    geom_point(size = 2, shape = 16, colour = "black", fill = "black") +
    lims(x = c(-1, 1), y = c(-1, 1)) +
    theme(aspect.ratio = 1)

  if (demo_status() == 1) {
    # show the player's first (proposed center) point
    plt <- plt + geom_point(
      aes(x = demo_player$points$x[1], y = demo_player$points$y[1]),
      shape = 19, color = "red", alpha = 0.9, size = 3
    )
  } else if (demo_status() >= 2) {
    req(nrow(demo_player$points) == 2)
    plt <- plt + geom_abline(
      slope = demo_player_slope(), intercept = demo_player_intercept(),
      color = "red", alpha = 0.9, size = 2
    )
  }

  if (isTRUE(input$demo_show_pc) && nrow(demo$data) >= 2) {
    # add the actual PC line
    plt <- plt +
      geom_point(aes(x = mean(demo$data$x), y = mean(demo$data$y)), shape = 19, color = "blue", alpha = 0.9, size = 4) +
      geom_abline(slope = demo_pc1_slope(), intercept = demo_pc1_intercept(), color = "blue", alpha = 0.9, size = 2)
  }
  if (isTRUE(input$demo_show_ellipse) && nrow(demo$data) >= 3) {
    plt <- plt + stat_ellipse(type = "norm", geom = "polygon", fill = "green", color = "black", alpha = 0.3, level = 0.39)
  }
  if (isTRUE(input$demo_show_pacman) && nrow(demo$data) >= 2) {
    pac_man <- compute_pac_man(demo_data_cov(), data.frame(x = mean(demo$data$x), y = mean(demo$data$y)))
    plt <- plt + geom_polygon(data = pac_man, fill = "yellow", color = "black", alpha = 0.5)
  }
  plt
})

# hint text below the plot, depending on demo status
observe({
  if (demo_status() == -1) {
    output$demo_info <- renderText("Click on plot to draw points.")
  } else if (demo_status() %in% c(0, 1)) {
    output$demo_info <- renderText("Draw two points by clicking on the plot to describe a line.")
  } else if (demo_status() == 2) {
    output$demo_info <- renderText(paste0(
      "Variance along proposed direction: ", round(demo_sigma_player(), 3),
      ". Variance along PC: ", round(demo_sigma_pc(), 3)
    ))
  }
})

# outputs -------------------------------------------------------------------

output$demo_plot <- renderPlot(demo_draw_points_reactive())
