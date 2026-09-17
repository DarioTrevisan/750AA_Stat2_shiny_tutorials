# game

# reactive for the randomly chosen dataset

observe({if(game_status()==-1){game_sample(sample_data_frame_numeric())
  game_status(0)}                             })

observeEvent(input$game_sample_points, game_status(-1))

#sample points
game_sample <- reactiveVal()

#name and column names
game_name_dataset <- reactive(game_sample()$name)
game_colnames <- reactive( colnames(game_data()) )


# do not standardize the data, otherwise since it is 2x2 it becomes trivial!

game_data <- reactive(game_sample()$data)

# compute the data mean
game_data_mean <- reactive(data.frame( x = mean(game_data()[,1]), y= mean(game_data()[,2])))

# compute covariance matrix
game_data_cov <- reactive(cov(game_data()))
sigma_player <- reactiveVal()
sigma_pc <- reactiveVal()

# compute first principal component

game_data_pc1 <- reactive(prcomp(game_data())$rotation[,1])
game_pc1_slope <- reactive(as.numeric(game_data_pc1()[2]/game_data_pc1()[1]))
game_pc1_intercept <- reactive( as.numeric(game_data_mean()$y- game_pc1_slope() *game_data_mean()$x))
# store point coordinates for the player submitted as reactive values

game_player <- reactiveValues()

game_player$points <- data.frame(x=numeric(), y=numeric())

game_player$slope <- numeric()
game_player$intercept <- numeric()



# plot
game_plot_reactive <- reactiveVal()

 
# game status:
# 0 expects player to click to propose mean of points
# 1 expects player to click for direction of PC line
# 2 shows the player the proposed PC line
# 3 shows the player the actual PC line and moves to next level

game_status <- reactiveVal(-1)

# 
# # game level
game_level <- reactiveVal(0)
# 
# # game aestetics
game_num_points <- reactiveVal(10)
game_color_fill <- reactiveVal()
game_color_edge <- reactiveVal()
# 
# # game statistics
# 
 game_diff_angles <- reactive( atan(game_pc1_slope() ) - atan(game_player$slope) )
 game_play_score <- reactiveVal(0) 
 game_level <- reactiveVal(1)
 game_average_score <- reactiveVal(0)
 game_total_score <- reactiveVal(0)
 
 game_average_score <- reactive(game_total_score()/(game_level()-1))
 

# 
# 


# 
# at single click 
# 

 # double click or confirm button puts the game in status 3 if there is a line by the player

 observeEvent(input$game_confirm_button, {if(nrow(game_player$points) ==2) game_status(3)} )
 observeEvent(input$game_plot_dblclick, {if(nrow(game_player$points) ==2)
  game_status(3)})

observeEvent(input$game_plot_click, {
  if (game_status() == 0) {
    game_player$points <- data.frame(
        x = input$game_plot_click$x,
        y = input$game_plot_click$y
    )
    game_status(1)
  }
  else if (game_status() ==1 ){
    game_player$points <- rbind( game_player$points, data.frame(
      x = input$game_plot_click$x,
      y = input$game_plot_click$y ))
    game_status(2)
  }
  else if (game_status()==3){
    #update the total score and the level
    game_total_score(game_total_score()+game_play_score())
    game_level(game_level()+1)
    game_player$points <- data.frame(x=numeric(), y=numeric())
    game_status(-1)
  }
  else {
    game_player$points <- data.frame( x=numeric(), y=numeric())
    game_status(0)
  }
})


# # plot points reactive
# 
 game_draw_points_reactive <- reactive({
   plt <- ggplot(game_data(), aes(x = game_data()[,1], game_data()[,2])) +
    geom_point(
      size = 2,
      shape = 16,
      colour = "black",
      fill = "black"
    ) +
     labs(x= game_colnames()[1], y=game_colnames()[2])+
     #labs(x= game_colnames()[1], y=game_colnames()[2])+
     theme(aspect.ratio = 1)
   if(game_status() == 0){
    # in this case simply plot the points
   }
   else if (game_status()==1){
     # add the user proposed center
     plt<- plt+geom_point(aes(x=game_player$points$x[1], y=game_player$points$y[1]), shape=19,  color="red",  alpha=0.9, size =3)
   }
   else if (game_status() >= 2){
     req(nrow(game_player$points)>1 )
     #compute level score
     M <- game_data_cov()
     point1 <- game_player$points[1,]  
     point2 <- game_player$points[2,]
     v <- as.numeric(point2-point1)
     v <- v/sqrt(sum(v**2))
     sigma_player(sqrt(sum( v * (M %*% v))))
     #output$debug <- verbatimTextOutput(v_player)
     sigma_pc(sqrt(sum( game_data_pc1() * (M %*% game_data_pc1() ) )))
     game_play_score(min(round(-1.1*log(1-sigma_player()/sigma_pc()), 0), 10))
     
     
     # add a line through the two proposed points
     point1 <- game_player$points[1,]  # (x1, y1)
     point2 <- game_player$points[2,]  # (x2, y2)
     # Calculate slope and intercept for the line
     game_player$slope <- as.numeric((point2[2] - point1[2]) / (point2[1] - point1[1]))
     game_player$intercept <- as.numeric(point1[2] - game_player$slope * point1[1])
     #plt <- plt+geom_point(aes(x=game_player$points$x[1], y=game_player$points$y[1]), shape=21, fill = "yellow", color="black", size =6)
     #plt <- plt+ geom_segment(aes(x = game_player$points$x[1], y = game_player$points$y[1], xend = game_player$points$x[2], yend = game_player$points$y[2]), arrow = arrow(type="closed"), colour="red") 
     plt <- plt +   geom_abline(slope = game_player$slope, intercept = game_player$intercept, color = "red",  alpha=0.9, size = 2)   # Add the line
     if (game_status()==3){
     # add the actual PC line
      plt <- plt+ 
        geom_point(aes(x=game_data_mean()$x, y=game_data_mean()$y), shape=19,  color="blue",  alpha=0.9, size =4)+
        geom_abline(slope=game_pc1_slope(), intercept=game_pc1_intercept(), color="blue", alpha=0.9, size=2)+
        stat_ellipse(type="norm", geom="polygon", fill='green', color="black", alpha=0.3, level=0.39)
     }
     
   }
   # dark theme: plt <- plt+theme_dark()
   plt
})
# 
# 
 
 observe({ if(game_status()==0 | game_status() ==1 ){
   output$game_hint <-  renderText("Draw two points by clicking on the plot to describe a line.")
 }
   if(game_status()==2){
     output$game_hint <- renderText("Click on plot to erase the line, double click or hit the button to confirm your choice.")
   }
   if(game_status()==3){
     output$game_hint <- renderText(paste0("Variance along proposed direction: ", round(sigma_player(), 3), ". Variance along PC: ", round(sigma_pc(), 3), ". Score ", game_play_score(), "/10.  Click on plot to move to the next level."))
   }
   })
 

 
# # outputs
output$game_plot <- renderPlot( game_draw_points_reactive() )
output$game_stats <- renderText( paste0("Average score: ", round(game_average_score(), 1), "/10." ))
 output$game_hint <- renderPrint( game_suggestion() )
 output$level_info <- renderText(paste0("Level ", game_level(), ". From dataset: ", game_name_dataset(), " (see tab for more info)."))
