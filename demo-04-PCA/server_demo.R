# demo

# reactive for the randomly chosen dataset



observe(if(input$radio==-1){demo_status(-1)} else if(input$radio==0){demo_status(0)})


demo <- reactiveValues()

# define a data frame for the data input

demo$data<- data.frame(x=numeric(), y=numeric())

# compute covariance matrix
demo_data_cov <- reactive(cov(demo$data))
sigma_player <- reactiveVal()
sigma_pc <- reactiveVal()

# compute first principal component

demo_data_pc1 <- reactive(prcomp(demo$data)$rotation[,1])
demo_pc1_slope <- reactive(as.numeric(demo_data_pc1()[2]/demo_data_pc1()[1]))
demo_pc1_intercept <- reactive( mean(demo$data$y)- demo_pc1_slope() *mean(demo$data$x)) 
# store point coordinates for the player submitted as reactive values

demo_player <- reactiveValues()

demo_player$points <- data.frame(x=numeric(), y=numeric())

demo_player$slope <- numeric()
demo_player$intercept <- numeric()



# plot
demo_plot_reactive <- reactiveVal()


# demo status:
# -1 expects player to click to draw data points
# 0 expects player to click to propose mean of points
# 1 expects player to click for direction of PC line
# 2 cancels the player's line


demo_status <- reactiveVal(-1)






# 
# at single click 
# 

observeEvent(input$demo_erase_plot, {demo$data <- data.frame(x=numeric(), y=numeric())
game_status(-1)} )


observeEvent(input$demo_plot_click, {
  if (demo_status() ==-1){
    add_points <- data.frame(x = input$demo_plot_click$x, y = input$demo_plot_click$y)
    demo$data <- rbind( demo$data, add_points)
    demo_player$points <- data.frame(x=numeric(), y=numeric())
    }
  else if (demo_status() == 0) {
    demo_player$points <- data.frame(
      x = input$demo_plot_click$x,
      y = input$demo_plot_click$y
    )
    demo_status(1)
  }
  else if (demo_status() ==1 ){
    demo_player$points <- rbind( demo_player$points, data.frame(
       x = input$demo_plot_click$x,
       y = input$demo_plot_click$y ))
    demo_status(2)
  }
  else {
    demo_player$points <- data.frame( x=numeric(), y=numeric())
    demo_status(0)
  }
})


# # plot points reactive
# 
demo_draw_points_reactive <- reactive({
  plt <- ggplot(demo$data, aes(x = x, y=y )) +
    geom_point(
      size = 2,
      shape = 16,
      colour = "black",
      fill = "black"
    ) + 
    lims(x = c(-1, 1), y = c(-1, 1)) +
    theme(aspect.ratio = 1)
  if (demo_status()==1){
    # add the user proposed center
    plt<- plt+geom_point(aes(x=demo_player$points$x[1], y=demo_player$points$y[1]), shape=19,  color="red",  alpha=0.9, size =3)
  }
  else if (demo_status() >= 2){
    req(nrow(demo_player$points)>1 )
    #compute level score
    M <- demo_data_cov()
    point1 <- demo_player$points[1,]  
    point2 <- demo_player$points[2,]
    v <- as.numeric(point2-point1)
    v <- v/sqrt(sum(v**2))
    sigma_player(sqrt(sum( v * (M %*% v))))
    sigma_pc(sqrt(sum( demo_data_pc1() * (M %*% demo_data_pc1() ) )))

    
    # add a line through the two proposed points
    point1 <- demo_player$points[1,]  # (x1, y1)
    point2 <- demo_player$points[2,]  # (x2, y2)
    # Calculate slope and intercept for the line
    demo_player$slope <- as.numeric((point2[2] - point1[2]) / (point2[1] - point1[1]))
    demo_player$intercept <- as.numeric(point1[2] - demo_player$slope * point1[1])
    #plt <- plt+geom_point(aes(x=demo_player$points$x[1], y=demo_player$points$y[1]), shape=21, fill = "yellow", color="black", size =6)
    #plt <- plt+ geom_segment(aes(x = demo_player$points$x[1], y = demo_player$points$y[1], xend = demo_player$points$x[2], yend = demo_player$points$y[2]), arrow = arrow(type="closed"), colour="red") 
    plt <- plt +   geom_abline(slope = demo_player$slope, intercept = demo_player$intercept, color = "red",  alpha=0.9, size = 2)   # Add the line
  }
  if (input$demo_show_pc){
    # add the actual PC line
    plt <- plt+ 
      geom_point(aes(x=mean(demo$data$x), y=mean(demo$data$y)), shape=19,  color="blue",  alpha=0.9, size =4)+
      geom_abline(slope=demo_pc1_slope(), intercept=demo_pc1_intercept(), color="blue", alpha=0.9, size=2)
  }
  if (input$demo_show_ellipse){
    plt <- plt + stat_ellipse(type="norm", geom="polygon", fill='green', color="black", alpha=0.3, level=0.39)
  }
  if (input$demo_show_pacman){
    pac_man <- compute_pac_man(demo_data_cov(), data.frame(x=mean(demo$data$x), y=mean(demo$data$y)))
    plt <- plt+geom_polygon(data=pac_man, fill="yellow", color="black", alpha=0.5)
  }
  # dark theme: plt <- plt+theme_dark()
  plt
})
# 
# 

observe({ if(demo_status()==-1){ output$demo_info <- renderText( "Click on plot to draw points.")}
  if(demo_status()==0 | demo_status() ==1 ){
  output$demo_info <-  renderText("Draw two points by clicking on the plot to describe a line.")
}
  if(demo_status()==2){
    output$demo_info <- renderText(paste0("Variance along proposed direction: ", round(sigma_player(), 3), ". Variance along PC: ", round(sigma_pc(), 3)))
  }
})



# # outputs
output$demo_debug <- renderPrint(demo_status())
 output$demo_plot <- renderPlot( demo_draw_points_reactive() )
# output$demo_hint <- renderPrint( demo_suggestion() )
# output$demo_info <- renderText(paste0("Level ", demo_level(), ". From dataset: ", demo_name_dataset(), " (see tab for more info)."))