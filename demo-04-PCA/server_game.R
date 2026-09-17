# game

# game status meanings:
#   -1 samples a new dataset and resets the round
#    0 expects the player to click to place the first point of their line
#    1 expects the player to click to place the second point of their line
#    2 shows the player's proposed line; click erases it, confirm/double-click locks it in
#    3 shows the actual PC1 line, the score, and waits for a click to move to the next level

game_status <- reactiveVal(-1)
game_level <- reactiveVal(1)
game_total_score <- reactiveVal(0)

game_sample <- reactiveVal(NULL)

# store the point coordinates submitted by the player
game_player <- reactiveValues(points = data.frame(x = numeric(), y = numeric()))

# sample a fresh dataset (and reset the player's points) whenever the round restarts
observe({
  if (game_status() == -1) {
    game_sample(sample_data_frame_numeric())
    game_player$points <- data.frame(x = numeric(), y = numeric())
    game_status(0)
  }
})

observeEvent(input$game_sample_points, game_status(-1))

# name of the currently sampled dataset, and its raw data
# (data is not standardized: since it is only 2 columns, standardizing would make it trivial)
game_name_dataset <- reactive({
  req(game_sample())
  game_sample()$name
})

game_data <- reactive({
  req(game_sample())
  game_sample()$data
})

game_colnames <- reactive(colnames(game_data()))

# standardized x/y data frame used only for plotting
game_data_xy <- reactive(data.frame(x = game_data()[, 1], y = game_data()[, 2]))

game_data_mean <- reactive(data.frame(x = mean(game_data()[, 1]), y = mean(game_data()[, 2])))
game_data_cov <- reactive(cov(game_data()))

# first principal component of the sampled data
game_data_pc1 <- reactive(prcomp(game_data())$rotation[, 1])
game_pc1_slope <- reactive(as.numeric(game_data_pc1()[2] / game_data_pc1()[1]))
game_pc1_intercept <- reactive(as.numeric(game_data_mean()$y - game_pc1_slope() * game_data_mean()$x))

# player's proposed direction, once both points are placed
game_player_direction <- reactive({
  req(nrow(game_player$points) == 2)
  v <- as.numeric(game_player$points[2, ] - game_player$points[1, ])
  v / sqrt(sum(v ^ 2))
})

game_player_slope <- reactive({
  v <- game_player_direction()
  as.numeric(v[2] / v[1])
})

game_player_intercept <- reactive({
  point1 <- game_player$points[1, ]
  as.numeric(point1$y - game_player_slope() * point1$x)
})

# variance along the player's direction and along the true PC1 direction
game_sigma_player <- reactive({
  v <- game_player_direction()
  sqrt(sum(v * (game_data_cov() %*% v)))
})

game_sigma_pc <- reactive({
  pc <- game_data_pc1()
  sqrt(sum(pc * (game_data_cov() %*% pc)))
})

# score in [0, 10]: 10 when the player's direction matches PC1, 0 when the
# proposed direction captures none of the variance
game_play_score <- reactive({
  pc_sigma <- game_sigma_pc()
  if (!is.finite(pc_sigma) || pc_sigma <= 0) return(0)
  ratio <- min(game_sigma_player() / pc_sigma, 1 - 1e-9)
  score <- -1.1 * log(1 - ratio)
  max(min(round(score, 0), 10), 0)
})

# levels only count once they have actually been completed and scored
game_levels_completed <- reactive(max(game_level() - 1, 0))

game_average_score <- reactive({
  completed <- game_levels_completed()
  if (completed == 0) 0 else game_total_score() / completed
})

# confirm button / double-click lock in the player's line once both points are placed
observeEvent(input$game_confirm_button, {
  req(nrow(game_player$points) == 2)
  game_status(3)
})

observeEvent(input$game_plot_dblclick, {
  req(nrow(game_player$points) == 2)
  game_status(3)
})

observeEvent(input$game_plot_click, {
  click <- data.frame(x = input$game_plot_click$x, y = input$game_plot_click$y)
  if (game_status() == 0) {
    game_player$points <- click
    game_status(1)
  } else if (game_status() == 1) {
    game_player$points <- rbind(game_player$points, click)
    game_status(2)
  } else if (game_status() == 3) {
    # lock in the score for this level and move on to the next one
    game_total_score(game_total_score() + game_play_score())
    game_level(game_level() + 1)
    game_status(-1)
  } else {
    # status 2: clicking anywhere on the plot erases the proposed line
    game_player$points <- data.frame(x = numeric(), y = numeric())
    game_status(0)
  }
})

# plot ----------------------------------------------------------------------

game_draw_points_reactive <- reactive({
  plt <- ggplot(game_data_xy(), aes(x = x, y = y)) +
    geom_point(size = 2, shape = 16, colour = "black", fill = "black") +
    labs(x = game_colnames()[1], y = game_colnames()[2]) +
    theme(aspect.ratio = 1)

  if (game_status() == 1) {
    # show the player's first point
    plt <- plt + geom_point(
      aes(x = game_player$points$x[1], y = game_player$points$y[1]),
      shape = 19, color = "red", alpha = 0.9, size = 3
    )
  } else if (game_status() >= 2) {
    req(nrow(game_player$points) == 2)
    plt <- plt + geom_abline(
      slope = game_player_slope(), intercept = game_player_intercept(),
      color = "red", alpha = 0.9, size = 2
    )
    if (game_status() == 3) {
      # reveal the true PC1 direction and the data ellipse
      plt <- plt +
        geom_point(
          aes(x = game_data_mean()$x, y = game_data_mean()$y),
          shape = 19, color = "blue", alpha = 0.9, size = 4
        ) +
        geom_abline(slope = game_pc1_slope(), intercept = game_pc1_intercept(), color = "blue", alpha = 0.9, size = 2) +
        stat_ellipse(type = "norm", geom = "polygon", fill = "green", color = "black", alpha = 0.3, level = 0.39)
    }
  }
  plt
})

# hint text below the plot, depending on game status
observe({
  if (game_status() %in% c(0, 1)) {
    output$game_hint <- renderText("Draw two points by clicking on the plot to describe a line.")
  } else if (game_status() == 2) {
    output$game_hint <- renderText("Click on plot to erase the line, double click or hit the button to confirm your choice.")
  } else if (game_status() == 3) {
    output$game_hint <- renderText(paste0(
      "Variance along proposed direction: ", round(game_sigma_player(), 3),
      ". Variance along PC: ", round(game_sigma_pc(), 3),
      ". Score ", game_play_score(), "/10.  Click on plot to move to the next level."
    ))
  }
})

# outputs ---------------------------------------------------------------

output$game_plot <- renderPlot(game_draw_points_reactive())
output$game_stats <- renderText(paste0("Average score: ", round(game_average_score(), 1), "/10."))
output$level_info <- renderText(paste0("Level ", game_level(), ". From dataset: ", game_name_dataset(), " (see tab for more info)."))
