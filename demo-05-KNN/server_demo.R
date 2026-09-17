#demo

# slider widget to choose k not larger than num points

demo_slide_max = reactive(min(10, max(1, nrow(
  demo$training_points
))))
demo_slide_min = 1

demo_slide_zoom_max = reactive(max(1, nrow(demo$training_points) - 1))


output$demo_slider_choose_k <- renderUI({
  sliderInput(
    "demo_k",
    "Choose k (<= training points)",
    min = demo_slide_min,
    max = demo_slide_max(),
    value = 1,
    step = 1
  )
})

output$demo_slider_zoom_k <- renderUI({
  sliderInput(
    "demo_zoom_k",
    "Show k up to value:",
    min = demo_slide_min,
    max = demo_slide_zoom_max(),
    value = demo_slide_zoom_max(),
    step = 1
  )
})


#reactive values

demo <- reactiveValues()

demo$training_points <- data.frame(x = numeric(), y = numeric(), class = factor())

demo$test_points <- data.frame(
  x = numeric(),
  y = numeric(),
  class = factor(),
  probability = numeric()
)

# at click add the points

demo_add_train_point <- reactive({
  add_row <- data.frame(
    x = input$demo_plot_click$x,
    y = input$demo_plot_click$y,
    class = as.factor(input$demo_class)
  )
  # add row to the data.frame
  demo$training_points <- rbind(demo$training_points, add_row)
})

observeEvent(input$demo_plot_click, if(input$demo_radio == "demo_train"){
  demo_add_train_point()
} 
else{
  demo_add_test_point()
}
)
             
demo_add_test_point <- reactive({
  add_row <- data.frame(
    x = input$demo_plot_click$x,
    y = input$demo_plot_click$y,
    class = NA,
    probability = NA
  )
  # add row to the data.frame
  demo$test_points <- rbind(demo$test_points, add_row)
})


# demo scatter plot

output$demo_plot = renderPlot({
  ggplot(demo$training_points, aes(x = x, y = y, )) +
    geom_point(aes(color = class, shape = class), size = 5) +
    geom_label(
      data = demo$test_points,
      aes(
        x = x,
        y = y,
        label = rownames(demo$test_points),
        fill = class
      ),
      size = 5,
      colour = "white",
      fontface = "bold"
    ) +
    lims(x = c(-1, 1), y = c(-1, 1)) +
   theme(aspect.ratio = 1, legend.position = "none")
})

# classify

observeEvent(input$demo_classify, {
  if(nrow(demo$training_points)>1){
  knn_output <- knn(
    demo$training_points[, 1:2],
    test = demo$test_points[, 1:2],
    cl = demo$training_points$class,
    k = input$demo_k,
    prob = TRUE
  )
  demo$test_points$class <- as.vector(knn_output)
  demo$test_points$probability <- attributes(knn_output)$prob
  }
})

# training points table output

output$demo_test_points <- renderTable(demo$test_points)

# errors

demo_training_error <- reactive({
  training_error <- numeric()
  if (nrow(demo$training_points) > 1) {
    for (k in 1:(nrow(demo$training_points) - 1)) {
      error <- mean(
        demo$training_points$class != knn(
          demo$training_points[, 1:2],
          demo$training_points[, 1:2],
          cl = demo$training_points$class,
          k
        )
      )
      training_error <- c(training_error, error)
    }
    data.frame(k = 1:(nrow(demo$training_points) -
                        1), error = training_error)
  }
  else {
    data.frame(k = numeric(), error = numeric())
  }
})

demo_cv_error <- reactive({
  cv_error <- numeric()
  if (nrow(demo$training_points) > 1) {
    for (k in 1:(nrow(demo$training_points) - 1)) {
      error <- mean(
        demo$training_points$class != knn.cv(demo$training_points[, 1:2], cl = demo$training_points$class, k)
      )
      cv_error <- c(cv_error, error)
    }
    data.frame(k = 1:(nrow(demo$training_points) - 1), error = cv_error)
  }
  else {
    data.frame(k = numeric(), error = numeric())
  }
})


# errors plot

output$demo_error <- renderPlot({
  if (input$demo_show_error) {
    ggplot(demo_training_error(), aes(x = k, y = error, color = "train")) +
      scale_y_continuous(labels = percent, limits =
                           c(0, 1)) +
      scale_x_continuous(breaks = pretty_breaks(),
                         limits = c(1, input$demo_zoom_k)) +
      geom_point(size = 4) +
      geom_line(linewidth = 2) +
      geom_line(data = demo_cv_error(),
                aes(x = k, y = error, colour = "cv"),
                linewidth = 2) +
      geom_point(data = demo_cv_error(),
                 aes(x = k, y = error, colour = "cv"),
                 size = 4) +
      scale_color_manual(values = c("train" = "blue", "cv" = "orange")) +
      ylab("error rate") +
      theme(legend.position = "top")
  }
})


# reset plot

observeEvent(
  input$demo_erase_plot,
  demo$training_points <- data.frame(x = numeric(), y = numeric(), class = factor())
)
observeEvent(
  input$demo_remove_test_points,
  demo$test_points <- data.frame(
    x = numeric(),
    y = numeric(),
    class = factor(),
    probability = numeric()
  )
)


observe({if(input$demo_radio == "demo_train"){
  output$demo_choice_click <- renderText("Click on the plot to add points of a chosen class (use slider below to set the class).")
}
  else{
    output$demo_choice_click <- renderText("Click on the plot to add points to be classified.")
  }
})