# game

# game status variable
# 0 means player can add points
# 1 means player cannot add points, only start/stop coin tossing and confirm target

game_status <- reactiveVal(0)

game <- reactiveValues()

game_level <- reactiveVal(1)
game_coins <- reactiveVal(10)

# define a data frame for the data input

game$prior_points <- data.frame(x = numeric(), y = numeric())
game$prior <- data.frame(x = numeric(), y = numeric())
game$likelihood <- data.frame(x = numeric(), y = numeric())
game$posterior <- data.frame(x = numeric(), y = numeric())
game$outcomes <- numeric()
game$maps <- numeric() 

# setup the y range for the plot

ymax <- reactive({
  if (nrow(game$prior_points) < 1) {
    2
  }
  else{
    max(2 * max(game$prior_points$y), 2)
  }
})

game_mode <- reactive(if (nrow(game$prior_points > 1))
  game$prior$x[which.max(game$prior$y)]
  else
    NA)


level_ordering <- sample(100, 100)

game_p <- reactive(values_p$value[level_ordering[game_level()]])
 
# observe({
#   if (input$game_show_mode == TRUE) {
#     output$game_mode_text <- renderText(paste0(
#       "The point of maximum density (mode, MAP) is ",
#       round(game_mode(), 3),
#       "."
#     ))
#   }
#   else{
#     output$game_mode_text <- renderText("")
#   }
# })

# 
 observeEvent(input$game_bayes, {
   if(game_status() < 2){
   #set the status to 1 to prevent further points drawn
   game_status(1)
   #single coin toss
  if(game_coins()>0){
     game_coins(game_coins()-1)
  coin_toss <- rbinom(1, 1, prob = game_p())
  game$outcomes <- c(game$outcomes, coin_toss) 
   if (nrow(game$prior_points) <2) {
     game$prior_points  <- data.frame(x = c(0,1), y = c(1,1))
     game$prior <- data.frame(x = grid, y = rep(1, length(grid)))
   }
   likelihood <- grid ** coin_toss * (1 - grid) ** (1-coin_toss)
   likelihood <- likelihood / max(likelihood)
   game$likelihood <- data.frame(x = grid, y = likelihood)

   game$maps <- c(game$maps, game_mode())
   posterior <- game$prior$y * likelihood
   game$prior <- data.frame(x = grid, y = posterior)
  }
   }
})


step = 0.01
grid <- seq(0, 1, by = step)
# 
# 
# observeEvent(input$game_bayes, {
#   if (nrow(game$prior_points) < 1) {
#     game$prior <- data.frame(x = grid, y = rep(1, length(grid)))
#   }
#   game$prior$y <- game$posterior$y
#   game$likelihood <- data.frame(x = numeric(), y = numeric())
#   #game$likelihood$y <- rep(1, length(grid))
#   game$outcomes <- numeric()
# })

# 
# observeEvent(input$game_normalize, {
#   if (nrow(game$prior_points) > 1) {
#     z_norm <- sum(game$prior$y) * step
#     game$prior$y <- game$prior$y / z_norm
#     game$prior_points$y <- game$prior_points$y / z_norm
#   }
# })

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





# plot
game_plot_reactive <- reactiveVal()




observeEvent(input$game_confirm, {
  if(game_status() < 2){
  game_coins(game_coins()+game_earning())
  output$game_level_outcome <- renderText(paste( "The actual value for p is ", game_p(), ": you earn ", game_earning(), "coins! Erase the plot to move the next level!"))
  game_status(2)
  }
  })




observeEvent(input$game_erase_plot, {
             if(game_status()==2){
                              game_level(game_level()+1)
             }
             game_status(0)
             game$maps <- numeric()
             game$prior_points <- data.frame(x = numeric(), y = numeric())
             game$prior <- data.frame(x = numeric(), y = numeric())
             game$likelihood <- data.frame(x = numeric(), y = numeric())
             game$posterior <- data.frame(x = numeric(), y = numeric())
             game$outcomes <- numeric()
             output$game_level_outcome <- renderText(paste( ""))
             })

observeEvent(input$game_remove_last_point, {if(game_status() == 0)
             game$prior_points <- game$prior_points[-nrow(game$prior_points), ] }
             )

observeEvent(input$game_plot_click, {if(game_status() == 0){
  add_points <- data.frame(x = input$game_plot_click$x,
                           y = input$game_plot_click$y)
  game$prior_points <- rbind(game$prior_points, add_points)}
})
# 

output$game_outcomes <- renderText(paste( "Last 10 outcomes: ", paste( game$outcomes[max(1, length(game$outcomes)-10):length(game$outcomes)] , collapse=";")))



# plot points reactive

game_draw_points_reactive <- reactive({
  plt <- ggplot(game$prior_points, aes(x = x, y = y)) +
    expand_limits(y = 0) +
    labs(x = "coin bias", y = "probability")+
    theme(aspect.ratio = 1)
  if (nrow (game$prior_points) <= 2) {
    plt <- plt + lims(x = c(0, 1), y = c(0, ymax()))
  }
  if (nrow(game$prior) > 1) {
    plt <- plt +
      geom_line(data = game$prior,
                           aes(x = x, y = y, colour = "prior"),
                           linewidth = 2)+
      geom_point(
        aes(x = game_mode(), y = 0),
        color = "red",
        shape = 3,
        size = 4
      )
  }
  if (game_status() ==0){  plt <- plt +  geom_point(
      size = 2,
      shape = 16,
      colour = "black",
      fill = "black"
    )}
  # if (input$game_show_likelihood == TRUE) {
  #   plt <- plt + geom_line(
  #     data = game$likelihood,
  #     aes(x = x, y = y, colour = "likelihood"),
  #     linewidth = 2
  #   )
  # }
  # if (input$game_show_posterior == TRUE) {
  #   plt <- plt + geom_line(
  #     data = game$posterior,
  #     aes(x = x, y = y, colour = "posterior"),
  #     linewidth = 2
  #   )
  # }
  plt <- plt + scale_color_manual(values = c(
    "prior" = "orange",
    likelihood = "blue",
    posterior = "green"
  )) +theme(legend.position="none", axis.text.y = element_blank() )
  plt
})

game_earning <- reactive( min(round(1/abs(game_p()-game_mode()), 0), 10))


# outputs
output$game_plot_path <- renderPlot( {if(length(game$maps) != 0)  
   ggplot( data.frame(tosses=1:length(game$maps), MAP = game$maps), aes(tosses, MAP))+geom_line(linetype='dotted', col = "orange", linewidth=4)+geom_line(col = "orange", linewidth=1)})
output$game_debug <- renderPrint(game$maps )
output$game_plot <- renderPlot(game_draw_points_reactive())
output$game_map <- renderText(paste("Current MAP estimate for p: ", game_mode() ))
output$game_level_info <- renderText(paste("Level: ", game_level(), values_p$description[level_ordering[game_level()]]))
output$game_coins_info <-renderText(   paste("You have ", game_coins(), "coins. Use them wisely!" ))