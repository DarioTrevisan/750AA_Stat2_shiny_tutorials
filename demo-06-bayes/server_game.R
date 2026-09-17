# game

# game status variable
# 0 means player can add points
# 1 means player cannot add points, only toss coins and confirm target
# 2 means player has confirmed their estimate for the current level

game_status <- reactiveVal(0)

game <- reactiveValues()

game_level <- reactiveVal(1)
game_coins <- reactiveVal(10)

# data frame holding the raw points the player clicked on the plot
game$prior_points <- data.frame(x = numeric(), y = numeric())
game$prior <- data.frame(x = numeric(), y = numeric())
game$likelihood <- data.frame(x = numeric(), y = numeric())
game$posterior <- data.frame(x = numeric(), y = numeric())
game$outcomes <- numeric()
game$maps <- numeric()

# setup the y range for the plot
game_ymax <- reactive({
  if (nrow(game$prior_points) < 1) {
    2
  } else {
    max(2 * max(game$prior_points$y), 2)
  }
})

game_mode <- reactive({
  if (nrow(game$prior_points) > 1) {
    game$prior$x[which.max(game$prior$y)]
  } else {
    NA
  }
})

# one random level ordering per session, covering every row of values_p
n_levels <- nrow(values_p)
level_ordering <- sample(n_levels, n_levels)

game_p <- reactive(values_p$value[level_ordering[game_level()]])

observeEvent(input$game_bayes, {
  # only lock the prior and spend a coin when a toss can actually happen;
  # otherwise (e.g. the player is out of coins) leave them free to keep
  # drawing their prior instead of getting stuck in a dead end.
  if (game_status() < 2 && game_coins() > 0) {
    game_status(1)
    game_coins(game_coins() - 1)

    if (nrow(game$prior_points) < 2) {
      game$prior_points <- data.frame(x = c(0, 1), y = c(1, 1))
      game$prior <- data.frame(x = grid, y = rep(1, length(grid)))
    }

    coin_toss <- rbinom(1, 1, prob = game_p())
    game$outcomes <- c(game$outcomes, coin_toss)

    likelihood <- grid ** coin_toss * (1 - grid) ** (1 - coin_toss)
    likelihood <- likelihood / max(likelihood)
    game$likelihood <- data.frame(x = grid, y = likelihood)

    posterior <- game$prior$y * likelihood
    posterior <- posterior / (sum(posterior) * step)
    game$prior <- data.frame(x = grid, y = posterior)

    # record the MAP estimate *after* incorporating this toss, so the path
    # plot shows the trajectory of belief updates in the right order
    game$maps <- c(game$maps, game_mode())
  }
})

observe({
  if (nrow(game$prior_points) > 1) {
    points_log <- data.frame(x = game$prior_points$x,
                              y = log(game$prior_points$y))
    spline_values <- c()
    for (x in grid) {
      spline_values <- c(spline_values,
                          splinefun(points_log, method = "natural")(x))
    }
    game$prior <- data.frame(x = grid, y = exp(spline_values))
  }
})

observeEvent(input$game_confirm, {
  # guard against confirming before any usable estimate exists (no prior
  # drawn yet), which would otherwise push NA into game_coins and corrupt
  # the player's coin balance for the rest of the session
  req(game_status() < 2, !is.na(game_mode()))
  game_coins(game_coins() + game_earning())
  output$game_level_outcome <- renderText(paste(
    "The actual value for p is ", game_p(), ": you earn ", game_earning(),
    "coins! Erase the plot to move the next level!"
  ))
  game_status(2)
})

observeEvent(input$game_erase_plot, {
  if (game_status() == 2) {
    game_level(min(game_level() + 1, n_levels))
  }
  game_status(0)
  game$maps <- numeric()
  game$prior_points <- data.frame(x = numeric(), y = numeric())
  game$prior <- data.frame(x = numeric(), y = numeric())
  game$likelihood <- data.frame(x = numeric(), y = numeric())
  game$posterior <- data.frame(x = numeric(), y = numeric())
  game$outcomes <- numeric()
  output$game_level_outcome <- renderText("")
})

observeEvent(input$game_remove_last_point, {
  if (game_status() == 0 && nrow(game$prior_points) > 0) {
    game$prior_points <- game$prior_points[-nrow(game$prior_points), ]
  }
})

observeEvent(input$game_plot_click, {
  if (game_status() == 0) {
    # clamp the click to a valid, strictly-positive probability so log(y) in
    # the spline fit above never sees 0 (which would produce -Inf/NaN and
    # break the prior curve).
    add_point <- data.frame(
      x = min(max(input$game_plot_click$x, 0), 1),
      y = max(input$game_plot_click$y, 1e-6)
    )
    game$prior_points <- rbind(game$prior_points, add_point)
  }
})

output$game_outcomes <- renderText(paste(
  "Last 10 outcomes: ", paste(tail(game$outcomes, 10), collapse = ";")
))

# plot points reactive

game_draw_points_reactive <- reactive({
  plt <- ggplot(game$prior_points, aes(x = x, y = y)) +
    expand_limits(y = 0) +
    labs(x = "coin bias", y = "probability") +
    theme(aspect.ratio = 1)
  if (nrow(game$prior_points) <= 2) {
    plt <- plt + lims(x = c(0, 1), y = c(0, game_ymax()))
  }
  if (nrow(game$prior) > 1) {
    plt <- plt +
      geom_line(data = game$prior,
                aes(x = x, y = y, colour = "prior"),
                linewidth = 2) +
      geom_point(
        aes(x = game_mode(), y = 0),
        color = "red",
        shape = 3,
        size = 4
      )
  }
  if (game_status() == 0) {
    plt <- plt + geom_point(
      size = 2,
      shape = 16,
      colour = "black",
      fill = "black"
    )
  }
  plt <- plt + scale_color_manual(values = c(
    "prior" = "orange",
    likelihood = "blue",
    posterior = "green"
  )) + theme(legend.position = "none", axis.text.y = element_blank())
  plt
})

game_earning <- reactive(min(round(1 / abs(game_p() - game_mode()), 0), 10))

# outputs
output$game_plot_path <- renderPlot({
  if (length(game$maps) != 0) {
    ggplot(data.frame(tosses = seq_along(game$maps), MAP = game$maps), aes(tosses, MAP)) +
      geom_line(linetype = "dotted", col = "orange", linewidth = 4) +
      geom_line(col = "orange", linewidth = 1)
  }
})
output$game_plot <- renderPlot(game_draw_points_reactive())
output$game_map <- renderText(paste("Current MAP estimate for p: ", game_mode()))
output$game_prior_hint <- renderText(values_p$description[level_ordering[game_level()]])
output$game_level_info <- renderText(paste("Level: ", game_level(), values_p$description[level_ordering[game_level()]]))
output$game_coins_info <- renderText(paste("You have ", game_coins(), "coins. Use them wisely!"))
