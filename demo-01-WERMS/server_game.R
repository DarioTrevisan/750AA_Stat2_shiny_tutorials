# game

# reactive values for the game

# data frame to store sample points
game_sample_points <- reactiveVal(data.frame(x = numeric(), y = numeric()))

# minimizer position
game_minimizer_point <- reactiveVal()

# player's chosen point (initialized at 0)
game_player_point <- reactiveVal(0)

# loss (to be randomly chosen at each level)
game_loss_choice <- reactiveVal("OLS")

# control value to alternate between game phases (0 -> only points, 1 -> points, loss and minimizer)
game_phase <- reactiveVal(0)

# level number and sample size for the current level
game_level <- reactiveVal(1)
game_num_points <- reactiveVal(1)

# win/loss tally
game_wins <- reactiveVal(0)
game_plays <- reactiveVal(0)


# reactives for the game

# sample points and plot
game_reactive_sample <- reactive({
  game_sample_points(data.frame(
    x = runif(game_num_points(), min = -10, max = 10),
    y = rep(0, game_num_points())
  ))
})

# compute the minimizer
game_reactive_minimizer <- reactive({
  req(nrow(game_sample_points()) > 0)
  game_minimizer_point(empirical_minimizer(game_sample_points()$x, game_loss_choice()))
})

# difference between minimizer and user's point
game_diff <- reactive({
  req(game_minimizer_point())
  round(abs(game_player_point() - game_minimizer_point()), 1)
})

# plot points

game_reactive_plot <- reactive({
  ymax <- max(vLoss(c(-10, 10), game_sample_points()$x, game_loss_choice()))
  plt <- ggplot(game_sample_points(), aes(x = x, y = y)) +
    geom_point(
      size = {if (game_num_points() < 10) {4} else {2}},
      shape = 21,
      fill = game_level() + 3,
      color = "black"
    ) +
    lims(x = c(-10, 10), y = c(0, ymax))
  if (game_phase() == 1) {
    plt <- plt +
      geom_point(
        x = game_player_point(),
        y = 0,
        size = 6,
        shape = 3,
        color = "red"
      ) +
      geom_function(
        fun = vLoss,
        args = list(points = game_sample_points()$x, choice = game_loss_choice()),
        colour = "darkgrey",
        size = 2
      ) +
      geom_point(
        x = game_minimizer_point(),
        y = 0,
        shape = 22,
        fill = "red",
        color = "black",
        size = 4
      )
  }
  plt
})

# general observers

observe({
  req(game_reactive_sample())
  game_reactive_minimizer()
})

observe(game_reactive_plot())

# level setup: sample size and loss function scale with the level
observeEvent(game_level(), {
  min_points <- as.integer(game_level() / 2) + 3
  max_points <- max(game_level(), 20)
  game_num_points(sample(min_points:max_points, size = 1))
  game_loss_choice(sample(c("OLS", "ABS", "HUB", "1QUART", "10PERC", "EXP"), size = 1))
})

# observes the player's click: first click proposes a minimizer, second click
# evaluates it, tallies the result, and (on a win) advances to the next level
observeEvent(input$game_click, {
  if (game_phase() == 1) {
    game_plays(game_plays() + 1)
    if (game_diff() < 1) {
      game_wins(game_wins() + 1)
      game_level(game_level() + 1)
    }
    game_phase(0)
  } else {
    game_player_point(round(input$game_click$x, 1))
    game_reactive_minimizer()
    game_phase(1)
  }
})

# outputs
output$game_level <- renderText(paste0("LEVEL: ", game_level()))
output$game_level_info <- renderText(paste0(
  "Number of points: ", game_num_points(),
  ",   Loss function: l(z)= ", loss_formula(game_loss_choice())
))

output$game_plot <- renderPlot(game_reactive_plot())
output$game_proposed_minimizer <- renderText({
  if (game_phase() == 1) {
    paste0("Your choice is: ", game_player_point())
  } else {
    paste0("Click the plot to choose your minimizer!")
  }
})
output$game_minimizer_description <- renderText({
  if (game_phase() == 1) {
    if (game_diff() < 1) {
      paste0(
        "The actual minimizer is ",
        round(game_minimizer_point(), 1),
        ", which is a distance of ",
        game_diff(),
        " <1 from the chosen point: click the plot to move to the next level!"
      )
    } else {
      paste0(
        "The actual minimizer is ",
        round(game_minimizer_point(), 1),
        ", which is a distance of ",
        game_diff(),
        " >1 from the chosen point: click the plot and try again!"
      )
    }
  } else {
    ""
  }
})
output$game_stats <- renderText(paste0(game_wins(), " wins out of ", game_plays(), " games."))
