#game


# if the game is set to
# -1 the player must click on the plot to get a new dataset. Click moves to status 0
# 0 the player must click to get a new test point. Click moves to status 1
# 1 the player must click to confirm his classification. Click moves to status 2
# 2 the player gets feedback about actual classification and scores. By clicking again it gets either a new dataset (new level, status -1) or a new test point (new round, status 0)

game_status <- reactiveVal(-1)
game_level <- reactiveVal(1)
game_round <- reactiveVal(1)
game_points <- reactiveVal(0)

game_points_global <- reactiveVal(3)


#reactive vals for points

#set number of test points for game

game_k <- reactiveVal(1)

max_rounds <- 20

game <- reactiveValues()

game$training_points <- data.frame(x = numeric(), y = numeric(), class = factor())
game$test_points <- data.frame(x = numeric(), y = numeric(), class = factor())
game$name_columns_dataset <- character()
game$factor_levels <- character()
game$name_dataset <- character()


knn_class <- reactiveVal(character())
game$test_error <- data.frame(k = numeric(), error = numeric())
game$train_error <- data.frame(k = numeric(), error = numeric())

# sample points when game_status is -1


observe({
  if (game_status() == -1) {
    output$game_over <- renderText("")
    game_status(0)
    game_round(1)
    output$game_points <- renderText("")
    z <- sample_data_frame_numeric_class()
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
    
  }
  else if (game_status() == 1) {
    output$game_test_points_table <- renderTable(game$test_points[game_round(), 1:2])
    output$game_hint <- renderText("Use the radio buttons to choose a class. Click again the plot to confirm!")
    knn_class(as.character(
      knn(
        game$training_points[, 1:2],
        game$test_points[game_round(), 1:2],
        cl = game$training_points[, 3],
        game_round()
      )
    ))
  }
  else {
    output$game_test_points_table <- renderTable(game$test_points[game_round(), ])
    output$game_hint <- renderText("Click again to move to the next stage/level!")
    
    if (input$game_radio_class == game$test_points[game_round(), 3] &&
        input$game_radio_class == knn_class()) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          input$game_radio_class,
          " applying correctly ",
          game_round(),
          "-NN. This is also the actual point class! One additional life awarded!"
        )
      )
      game_points(1)
    }
    else if (input$game_radio_class == knn_class() &&
             knn_class() != game$test_points[game_round(), 3]) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          input$game_radio_class,
          " applying correctly ",
          game_round(),
          "-NN. However, the actual point class is ",
          game$test_points[game_round(), 3],
          ". No lives awarded!"
        )
      )
      game_points(0)
    }
    else if (input$game_radio_class != knn_class() &&
             input$game_radio_class == game$test_points[game_round(), 3]) {
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          input$game_radio_class,
          " but ",
          game_round(),
          "-NN yields the class ",
          knn_class(),
          ".
                                                But you still guessed the actual class of the point! No lives lost!"
        )
      )
      game_points(0)
    }
    else{
      output$game_points_info <- renderText(
        paste0(
          "You classified the point as ",
          input$game_radio_class,
          " but ",
          game_round(),
          "-NN yields the class ",
          knn_class(),
          ".
                                                Your guess is not even the actual class of the point, which is ",
          game$test_points[game_round(), 3],
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
    game_round(0)
    game_points_global(2)
  }
  else if (game_status() == 0) {
    game_status(1)
  }
  else  if (game_status() == 1) {
    game_status(2)
  }
  else {
    game_points_global(game_points_global() + game_points())
    if (game_round() < min(game_level(), max_rounds)) {
      game_status(0)
      game_round(game_round() + 1)
    }
    else{
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
  }
  else if (game_status() == 2) {
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
  plt <- plt +  labs(
    x = game$name_columns_dataset[1],
    y = game$name_columns_dataset[2],
    colour = game$name_columns_dataset[3],
    shape = game$name_columns_dataset[3]
  ) #+
    #theme(aspect.ratio = 1, legend.position = "top")
  plt
})


# output

output$game_plot <- renderPlot(game_plot_reactive())

# d

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
    game_round(),
    "-NN. Player lives: ",
    game_points_global(),
    "."
  )
)


## extra: error plots to show at the end (or beginning of level)


game_training_error <- reactive({
  training_error <- numeric()
  k_max <- max(1, game_level())#%(nrow(game$training_points)-1))
  if (nrow(game$training_points) > 1) {
    for (k in 1:k_max) {
      error <- mean(game$training_points[, 3] != as.character(
        knn(
          game$training_points[, 1:2],
          game$training_points[, 1:2],
          cl = game$training_points[, 3],
          k
        )
      ))
      training_error <- c(training_error, error)
    }
    data.frame(k = 1:k_max, error = training_error)
  }
  else {
    data.frame(k = numeric(), error = numeric())
  }
})

game_cv_error <- reactive({
  cv_error <- numeric()
  k_max <- max(1, game_level())# nrow(game$training_points)-1)
  for (k in 1:k_max) {
    error <- mean(game$training_points[, 3] != as.character(
      knn.cv(game$training_points[, 1:2], cl = game$training_points[, 3], k)
    ))
    cv_error <- c(cv_error, error)
  }
  data.frame(k = 1:k_max, error = cv_error)
})

game_test_error <- reactive({
  test_error <- numeric()
  k_max <- max(1, game_level())# nrow(game$training_points)-1)
  for (k in 1:k_max) {
    error <- mean(game$test_points[, 3] != as.character(
      knn(
        game$training_points[, 1:2],
        game$test_points[, 1:2],
        cl = game$training_points[, 3],
        k
      )
    ))
    test_error <- c(test_error, error)
  }
  data.frame(k = 1:k_max, error = test_error)
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

#output$game_debug <- renderText(paste0("game status: ", game_status())) # " \n computed class", knn_class(), " \n actual class: ", game$test_points[game_round(), 3]) )
output$game_debug_table <-  renderTable(game$training_error)