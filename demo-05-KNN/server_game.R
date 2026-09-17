# game

# game_status meaning:
#  -2  game over: click on the plot restarts the game (resets lives/level, goes to -1)
#  -1  a new dataset is sampled automatically for the level, then status moves to 0
#   0  waiting for a click to reveal a new test point to classify (click -> status 1)
#   1  a test point is shown; choose a class with the radio buttons, then click
#      the plot to confirm your answer (click -> status 2)
#   2  feedback on the confirmed answer is shown; clicking again advances to a
#      new test point (status 0) or, if the level is complete, a new dataset/level (status -1)

game_status <- reactiveVal(-1)
game_level <- reactiveVal(1)
game_round <- reactiveVal(1)
game_points <- reactiveVal(0)

game_points_global <- reactiveVal(3)

# the class the player picked, snapshotted at the moment they confirm (click
# while game_status == 1), so later edits to the radio button can't change
# an already-confirmed answer's outcome
game_answer <- reactiveVal(NULL)

# number of test points sampled for the game
max_rounds <- 20

game <- reactiveValues()

game$training_points <- data.frame(x = numeric(), y = numeric(), class = factor())
game$test_points <- data.frame(x = numeric(), y = numeric(), class = factor())
game$name_columns_dataset <- character()
game$factor_levels <- character()
game$name_dataset <- character()

knn_class <- reactiveVal(character())

# helpers: the number of available training points, and safe k values that
# never exceed what knn()/knn.cv() can handle for the current training set
# (prevents crashes on datasets too small for the current level/round)

game_train_n <- reactive(nrow(game$training_points))

game_effective_k <- reactive({
  n <- game_train_n()
  if (n < 2) 1 else min(game_round(), n - 1)
})

game_k_max <- reactive({
  n <- game_train_n()
  if (n < 2) 1 else min(game_level(), n - 1)
})

# sample a new dataset when game_status is -1

observe({
  if (game_status() == -1) {
    output$game_over <- renderText("")
    game_status(0)
    game_round(1)
    game_answer(NULL)

    # keep sampling until the dataset has enough rows for max_rounds test
    # points plus a reasonable training set (avoids NA-padded train/test
    # data and knn() errors on datasets that are too small)
    repeat {
      z <- sample_data_frame_numeric_class()
      if (nrow(z$data) >= max_rounds + 5) break
    }
    z_row <- nrow(z$data)
    z_max <- min(50, z_row)
    sample_total <- sample(z_row, z_max)
    game$training_points <- z$data[sample_total[-1:-max_rounds], ]
    game$test_points <- z$data[sample_total[1:max_rounds], ]
    game$name_columns_dataset <- colnames(z$data)
    game$name_dataset <- z$name
    game$factor_levels <- levels(z$data[, 3])
  }
})

observe({
  if (game_points_global() < 0) {
    output$game_over <- renderText("GAME OVER! Click on the plot to start a new game!")
    game_status(-2)
    game_points_global(0)
  }
  if (game_status() == 0) {
    output$game_hint <- renderText("Click on the plot to get a new test point to classify!")
    output$game_points_info <- renderText("")
    output$game_test_points_table <- renderTable(game$test_points[c(), ])
  } else if (game_status() == 1) {
    output$game_test_points_table <- renderTable(game$test_points[game_round(), 1:2])
    output$game_hint <- renderText("Use the radio buttons to choose a class. Click again the plot to confirm!")
    knn_class(as.character(
      knn(
        game$training_points[, 1:2],
        game$test_points[game_round(), 1:2],
        cl = game$training_points[, 3],
        k = game_effective_k()
      )
    ))
  } else if (game_status() == 2) {
    output$game_test_points_table <- renderTable(game$test_points[game_round(), ])
    output$game_hint <- renderText("Click again to move to the next stage/level!")

    req(game_answer())
    player_answer <- game_answer()
    actual_class <- as.character(game$test_points[game_round(), 3])

    if (player_answer == actual_class && player_answer == knn_class()) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          player_answer,
          " applying correctly ",
          game_effective_k(),
          "-NN. This is also the actual point class! One additional life awarded!"
        )
      )
      game_points(1)
    } else if (player_answer == knn_class() && knn_class() != actual_class) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          player_answer,
          " applying correctly ",
          game_effective_k(),
          "-NN. However, the actual point class is ",
          actual_class,
          ". No lives awarded!"
        )
      )
      game_points(0)
    } else if (player_answer != knn_class() && player_answer == actual_class) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          player_answer,
          " but ",
          game_effective_k(),
          "-NN yields the class ",
          knn_class(),
          ". But you still guessed the actual class of the point! No lives lost!"
        )
      )
      game_points(0)
    } else {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          player_answer,
          " but ",
          game_effective_k(),
          "-NN yields the class ",
          knn_class(),
          ". Your guess is not even the actual class of the point, which is ",
          actual_class,
          ". You lost a life!"
        )
      )
      game_points(-1)
    }
  }
})

observeEvent(input$game_plot_click, {
  if (game_status() == -2) {
    game_status(-1)
    game_level(1)
    game_points_global(3)
  } else if (game_status() == 0) {
    game_status(1)
  } else if (game_status() == 1) {
    # lock in the player's answer at confirm time, so later radio button
    # changes can no longer affect the outcome of this round
    game_answer(input$game_radio_class)
    game_status(2)
  } else {
    game_points_global(game_points_global() + game_points())
    game_answer(NULL)
    if (game_round() < min(game_level(), max_rounds)) {
      game_status(0)
      game_round(game_round() + 1)
    } else {
      game_level(game_level() + 1)
      game_status(-1)
    }
  }
})

# plot reactive

game_plot_reactive <- reactive({
  plt <- ggplot(
    game$training_points,
    aes(
      x = game$training_points[, 1],
      y = game$training_points[, 2],
      colour = game$training_points[, 3],
      shape = game$training_points[, 3]
    )
  ) +
    geom_point(size = 5, position = position_jitter(seed = 42))
  if (game_status() == 1) {
    plt <- plt + geom_point(
      aes(x = game$test_points[game_round(), 1], y = game$test_points[game_round(), 2]),
      size = 6,
      color = "black",
      shape = 8
    )
  } else if (game_status() == 2) {
    plt <- plt + geom_point(
      aes(
        x = game$test_points[game_round(), 1],
        y = game$test_points[game_round(), 2],
        color = game$test_points[game_round(), 3],
        shape = game$test_points[game_round(), 3]
      ),
      size = 5
    )
  }
  plt <- plt + labs(
    x = game$name_columns_dataset[1],
    y = game$name_columns_dataset[2],
    colour = game$name_columns_dataset[3],
    shape = game$name_columns_dataset[3]
  )
  plt
})

# output

output$game_plot <- renderPlot(game_plot_reactive())

output$game_radio_choose_class <- renderUI({
  if (length(game$factor_levels) > 0)
    radioButtons(
      inputId = "game_radio_class",
      label = "Choose a class:",
      choices = as.list(game$factor_levels)
    )
})

output$game_info <- renderText(
  paste0(
    "Level ",
    game_level(),
    ", stage ",
    game_round(),
    ": classify with ",
    game_effective_k(),
    "-NN. Player lives: ",
    game_points_global(),
    "."
  )
)

## extra: error plots to show at the end (or beginning of level)

game_training_error <- reactive({
  training_error <- numeric()
  k_max <- game_k_max()
  if (nrow(game$training_points) > 1) {
    for (k in 1:k_max) {
      error <- mean(game$training_points[, 3] != as.character(
        knn(
          game$training_points[, 1:2],
          game$training_points[, 1:2],
          cl = game$training_points[, 3],
          k = k
        )
      ))
      training_error <- c(training_error, error)
    }
    data.frame(k = 1:k_max, error = training_error)
  } else {
    data.frame(k = numeric(), error = numeric())
  }
})

game_cv_error <- reactive({
  cv_error <- numeric()
  k_max <- game_k_max()
  if (nrow(game$training_points) > 1) {
    for (k in 1:k_max) {
      error <- mean(game$training_points[, 3] != as.character(
        knn.cv(game$training_points[, 1:2], cl = game$training_points[, 3], k = k)
      ))
      cv_error <- c(cv_error, error)
    }
    data.frame(k = 1:k_max, error = cv_error)
  } else {
    data.frame(k = numeric(), error = numeric())
  }
})

game_test_error <- reactive({
  test_error <- numeric()
  k_max <- game_k_max()
  if (nrow(game$training_points) > 1) {
    for (k in 1:k_max) {
      error <- mean(game$test_points[, 3] != as.character(
        knn(
          game$training_points[, 1:2],
          game$test_points[, 1:2],
          cl = game$training_points[, 3],
          k = k
        )
      ))
      test_error <- c(test_error, error)
    }
    data.frame(k = 1:k_max, error = test_error)
  } else {
    data.frame(k = numeric(), error = numeric())
  }
})

# error plot

output$game_error <- renderPlot(
  ggplot(game_training_error(), aes(
    x = k, y = error, color = "train"
  )) +
    scale_y_continuous(labels = percent, limits = c(0, 1)) +
    scale_x_continuous(breaks = pretty_breaks()) +
    geom_point(size = 4) +
    geom_line(linewidth = 2) +
    geom_line(
      data = game_cv_error(),
      aes(x = k, y = error, color = "cv"),
      linewidth = 2
    ) +
    scale_color_manual(values = c(
      "train" = "blue",
      "cv" = "orange",
      "test" = "red"
    )) +
    geom_point(
      data = game_cv_error(),
      aes(x = k, y = error, color = "cv"),
      size = 4
    ) +
    geom_line(
      data = game_test_error(),
      aes(x = k, y = error, color = "test"),
      linewidth = 2
    ) +
    geom_point(
      data = game_test_error(),
      aes(x = k, y = error, color = "test"),
      size = 4
    ) +
    ylab("error rate") +
    theme(legend.position = "top")
)

# debug

output$game_debug_table <- renderTable(game_training_error())
