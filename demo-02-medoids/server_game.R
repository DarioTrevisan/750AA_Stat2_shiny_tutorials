# game

# reactive values for the game

game <- reactiveValues()

# data frame to store sample points
game$sample_points <- data.frame(x = numeric(), y = numeric())

# actual medoid index
game_medoid_point <- reactive(compute_medoid(game$sample_points))

# player's chosen point index
game_player_point <- reactiveVal()

# plot
game_plot_reactive <- reactiveVal()


# game status: 0 = start a new level, 1 = waiting for a selection,
# 2 = selection confirmed, showing the answer
game_status <- reactiveVal(0)

# game level
game_level <- reactiveVal(0)

# game aesthetics
game_num_points <- reactiveVal()
game_color_fill <- reactiveVal()
game_color_edge <- reactiveVal()

# game statistics

game_guess_correct <- reactiveVal(0)
game_guess_wrong <- reactiveVal(0)


# helper: randomly sample the number of points for the current level

sample_game_num_points <- function() {
  min_points <- as.integer(game_level() / 2) + 3
  n <- sample(min_points:(game_level() + 3), size = 1)
  game_num_points(n)
}

# helper: move on from the "showing the answer" state to a new round -
# remove the identified medoid, reset the player's selection (it is no
# longer valid once a point is removed) and get ready for the next pick
game_advance_round <- function() {
  game_status(1)
  game$sample_points <- game$sample_points[-game_medoid_point(), ]
  game_player_point(NULL)
  output$game_info <- renderText("Click to select a point. Double click (or hit button) to confirm.")
  game_plot_reactive(game_draw_points_reactive())
}

# helper: confirm the player's current choice (called from the confirm
# button and from double-clicking the plot)
game_confirm_choice <- function() {
  if (is.null(game_player_point())) {
    output$game_info <- renderText("Select a point first!")
    return(invisible(NULL))
  }

  if (game_status() == 1) {
    game_status(2)
    plt <- game_draw_points_reactive() +
      geom_point(
        aes(x = x, y = y),
        data = game$sample_points[game_player_point(), ],
        colour = "black",
        size = 7,
        shape = 21,
        fill = "white",
        alpha = 0.8
      ) +
      geom_point(
        aes(x = x, y = y),
        data = game$sample_points[game_medoid_point(), ],
        colour = "red",
        size = 10,
        shape = 10
      )
    game_plot_reactive(plt)

    if (game_medoid_point() == game_player_point()) {
      game_guess_correct(game_guess_correct() + 1)
      output$game_info <- renderText("Your guess is right! click again (or hit button) to continue.")
    } else {
      game_guess_wrong(game_guess_wrong() + 1)
      output$game_info <- renderText("Your guess is wrong! click again (or hit button) to continue.")
    }
  } else if (game_status() == 2) {
    game_advance_round()
  }
}


# observe events for the game

# if game status is 0, sample points, draw the plot and move to game status 1
# also randomly sample a color theme for the tiles (just to make it nicer)

observe({
  if (game_status() == 0) {
    output$game_info <- renderText("Click to select a point. Double click (or hit button) to confirm.")
    game_level(game_level() + 1)
    sample_game_num_points()
    game_color_fill(sample(1:30, 1))
    game_color_edge(sample(1:30, 1))
    game_player_point(NULL)
    game$sample_points <- data.frame(
      x = runif(game_num_points()),
      y = runif(game_num_points())
    )
    game_status(1)
    game_plot_reactive(game_draw_points_reactive())
  }
})

# if there are only two points left, move to the next level

observe({
  if (nrow(game$sample_points) == 2) {
    game_status(0)
  }
})


# at single click: propose medoid (status 1) or advance the round (status 2)

observeEvent(input$game_plot_click, {
  if (game_status() == 1) {
    output$game_info <- renderText("Click to select a point. Double click (or hit button) to confirm.")
    game_player_point(closest_point(
      data.frame(
        x = input$game_plot_click$x,
        y = input$game_plot_click$y
      ),
      game$sample_points
    ))
    game_plot_reactive(
      game_draw_points_reactive() + geom_point(
        aes(x = x, y = y),
        data = game$sample_points[game_player_point(), ],
        colour = "black",
        size = 7,
        shape = 21,
        fill = "white",
        alpha = 0.8
      )
    )
  } else if (game_status() == 2) {
    game_advance_round()
  }
})

observeEvent(input$game_confirm, game_confirm_choice())

observeEvent(input$game_plot_dblclick, game_confirm_choice())


# plot points reactive

game_draw_points_reactive <- reactive({
  ggplot(game$sample_points, aes(x = x, y = y)) +
    geom_point(
      size = 5,
      shape = 22,
      colour = game_color_edge(),
      fill = game_color_fill()
    ) +
    lims(x = c(0, 1), y = c(0, 1)) +
    theme(aspect.ratio = 1)
})


# outputs
output$game_plot <- renderPlot(game_plot_reactive())
output$level_info <- renderText(paste0("Level ", game_level()))
output$game_stats <- renderText({
  total_guesses <- game_guess_correct() + game_guess_wrong()
  success_rate <- if (total_guesses == 0) 0 else round(game_guess_correct() / total_guesses, 2) * 100
  paste0(
    "Right: ", game_guess_correct(),
    " times / Wrong: ", game_guess_wrong(),
    " times. Success rate: ", success_rate, "%."
  )
})
