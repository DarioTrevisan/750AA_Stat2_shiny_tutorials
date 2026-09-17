# demo 

# reactive values for the demo

demo_click_switch <- reactiveVal(0)

# data frame to store sample points
demo_sample_points <- reactiveVal(data.frame( x=numeric(), y=numeric()))

# minimizer position
demo_minimizer_point <- reactiveVal()

# player's chosen point (initialized at 0)
demo_player_point <- reactiveVal(0)

# plot
demo_plot_reactive <- reactiveVal()


# reactives for the demo

# sample points and plot
demo_reactive_sample <- reactive(
  demo_sample_points(data.frame(x = runif(input$demo_num_points, min = -10, max = 10),
                                     y = rep(0, input$demo_num_points)))
                                                                   )

# compute the minimizer
demo_reactive_minimizer <- reactive({
  demo_minimizer_point(                 
    empirical_minimizer(demo_sample_points()$x, input$demo_loss_choice) )
  }
  )

# difference between minimizer and user's point
demo_diff <- reactive(
  {req(demo_reactive_minimizer)
    round(abs(demo_player_point() - demo_minimizer_point()), 1)})

# plot points

demo_reactive_plot <- reactive({
  ymax <- max(vLoss(c(-10, 10), demo_sample_points()$x, input$demo_loss_choice))
  plt <-ggplot(demo_sample_points(), aes(x = x, y = y)) +
    geom_point(size = {if (input$demo_num_points <10) {4} else {2}},
               shape = 21,
               fill = "blue",
               colour="black")+
    geom_point(
      x = demo_player_point(),
      y = 0,
      size = 6,
      shape = 3,
      color = "red")+
    lims(x = c(-10, 10), y = c(0, ymax))
  if (input$demo_show_loss) {
    plt <- plt + geom_function(
      fun = vLoss,
      args = list(points = demo_sample_points()$x, choice = input$demo_loss_choice),
      colour = "darkgrey",
      size = 2
    )}
  if (input$demo_show_minimizer ) {
    plt <- plt + geom_point(
      x = demo_minimizer_point(),
      y = 0,
      shape = 22,
      fill = "red",
      color = "black",
      size = {if (game_num_points() <10) {4} else {2}}
    )
  }
  plt
})

# general observers

observe(demo_reactive_sample())
observe(#{req(demo_reactive_sample())
  demo_reactive_minimizer())
observe(demo_reactive_plot())

# observes the player chosen point
observeEvent(input$demo_click,  {demo_player_point(round(input$demo_click$x, 1))}
  )

# observes the sample points button
observeEvent(input$demo_button_sample, {
  demo_reactive_sample()
  demo_reactive_minimizer()
  })

# outputs
output$demo_plot <- renderPlot(demo_reactive_plot())
output$demo_choice_description <- renderText(paste0("Loss function: l(z)= ", loss_formula(input$demo_loss_choice)))
output$demo_proposed_minimizer <- renderText(paste0("Your choice is: ", demo_player_point() ))
output$demo_minimizer_description <- renderText({
  if(input$demo_show_minimizer){
    if (demo_diff() < 1) {
            paste0(
              "The actual minimizer is ",
              round(demo_minimizer_point(), 1),
              ", which is a distance of ",
              demo_diff(),
              " <1 from the chosen point: you WIN!"
            )
          }
          else {
            paste0(
              "The actual minimizer is ",
              round(demo_minimizer_point(), 1),
              ", which is a distance of ",
              demo_diff(),
              " >1 from the chosen point: try again!"
            )

          }
        }
        else {
          ""
        }
  })



# description of the featured loss functions
# 
# choice_word <- reactive({
#   if (choice() == "OLS") {
#     "loss(z) = z^2."
#   }
#   else if (choice() == "ABS") {
#     "loss(z) = |z|."
#   }
#   else if (choice() == "1QUART") {
#     "loss(z) = 3 z^- + z^+"
#   }
#   else if (choice() == "10PERC") {
#     "loss(z) = 9 z^- + z^+"
#   }
#   else if (choice() == "EXP") {
#     "loss(z) = exp(|z|)"
#   }
# })




# 
# 
# 
# # keep the minimizer and the plot updated
# observe(sample())
# observe(  sample_level())
# 
# observe( { req(sample())
#   update_minimizer()})
# 
# 
# 
# 
# observe( { req(sample_level())
#   update_minimizer()})
# observe(plot_points())
# observe(  plot_update() )
# observe( updateCheckboxInput(session, "show_minimizer", value=reactive_show_minimizer()))
# 
# 
# 
# 
# # text output according to the game result 
# 
# output$game_choice <- renderText(choice_word())
# output$demo_choice <- renderText(choice_word())
# 
# output$demo_minimizer <- renderText({
#   req(sample())
#   if (reactive_show_minimizer()) {
#     if (diff() < 1) {
#       paste0(
#         "The actual minimizer is ",
#         round(minimizer_position(), 1),
#         ", which is a distance of ",
#         diff(),
#         " <1 from the chosen point (", user_point(), "): you WIN!"
#       )
#     }
#     else {
#       paste0(
#         "The actual minimizer is ",
#         round(minimizer_position(), 1),
#         ", which is a distance of ",
#         diff(),
#         " >1 from the chosen point  (", user_point(), "): try again!"
#       )
#       
#     }
#   }
#   else {
#     ""
#   }
# })
# 
# # 
# output$game_minimizer <- renderText({
#   req(sample_level())
#   if (reactive_show_minimizer()) {
#     if (diff_level() < 1) {
#       paste0(
#         "The actual minimizer is ",
#         round(minimizer_position(), 1),
#         ", which is a distance of ",
#         diff_level(),
#         " <1 from the chosen point (", user_point(), "): you WIN! Click for next level!")
#       #level_game(level_game()+1)
#     }
#     else {
#       paste0(
#         "The actual minimizer is ",
#         round(minimizer_position(), 1),
#         ", which is a distance of ",
#         diff_level(),
#         " >1 from the chosen point  (", user_point(), "): click to try again!"
#       )
#       
#     }
#   }
#   else {
#     ""
#   }
# })
# 
# 
# 
# 
# 
# output$proposed_minimizer <- renderText({
#   if (      switch_game() == TRUE)
#     paste0("Your choice is: ", user_point())
#   else
#     paste0("Click on plot to propose a minimizer!")
# })
# 
# 
# 
# 
# sample_level <- reactive({
#   user_point(0)
#   num = level_game() %%5 +1
#   rand_choice =c(
#     "OLS" ,
#     "ABS",
#     "1QUART",
#     "10PERC",
#     "EXP"
#   )[num] 
#   choice(rand_choice )
#   reactive_show_minimizer(FALSE)
#   values$sample_points <- data.frame(x = runif(level_game(), min = -10, max = 10),
#                                      y = rep(0, level_game()))
#   
# })
# 
# 
# 
# ## Start the game
# 
# observeEvent(input$start_game, {
#   plays(0)
#   wins(0)
#   level_game(input$num_points)
#   sample_level()
#   switch_game(FALSE)
# })
# 
# 
# ## Game dynamics triggered by double click
# #  
# # play <- reactive({
# #    reactive_show_minimizer(TRUE)
# #                        level_game(level_game()+1)
# #                    sample_level()})
# # 
# # observe(level_game())
# 
# 
# # observe(level_game())
# #  observe(diff_level())
# 
# 
# ## Double click on plot shows minimizer
# 
# observeEvent(input$demo_show_minimizer, reactive_show_minimizer(TRUE)
# ) #output$prova<-renderText("ciao"))
# 
# observeEvent(input$show_minimizer, reactive_show_minimizer(input$show_minimizer))
# 
# observeEvent(input$game_click,   {
#   if( switch_game()==FALSE)
#   {reactive_show_minimizer(TRUE)
#     Sys.sleep(0)
#     switch_game(TRUE)
#     #if (diff_level() <1){
#     #  level_game(level_game()+1)
#     #  }
#   }
#   else {
#     #if (diff_level() <1){
#     level_game(level_game()+1)
#     #}
#     #else{
#     #  level_game(level_game())
#     #}
#     switch_game(FALSE)
#     sample_level()
#   }
# })

# observe(play())

# 
# 
# plot_points <- reactive({
#   ymax <- vLoss(10, values$sample_points$x, choice())
#   plt <-ggplot(values$sample_points, aes(x = x, y = y)) +
#     geom_point(size = 4,
#                shape = 16,
#                color = "blue")+
#     lims(x = c(-10, 10), y = c(0, ymax))
#   if (input$show_loss) {
#     plt <- plt + geom_function(
#       fun = vLoss,
#       args = list(points = values$sample_points$x, choice = choice()),
#       colour = "darkgrey",
#       size = 2
#     )}
#   if (reactive_show_minimizer() ) {
#     plt <- plt + geom_point(
#       x = minimizer_position(),
#       y = 0,
#       shape = 15,
#       color = "red",
#       size = 6
#     )
#   }
#   plot_reactive(plt)
#   plt
# })
# 
# # plot_update <- reactive( {
# #                   plt <- plot_points()+
# #                                         geom_point(
# #   x = user_point(),
# #   y = 0,
# #   size = 6,
# #   shape = 3,
# #   color = "red")
# #               plot_reactive(plt)
# #               plt
# #               })
# 
# plot_update <- reactive( {
#   plt <- plot_points()
#   if (switch_game() ){
#     plt <- plt +
#       geom_point(
#         x = user_point(),
#         y = 0,
#         size = 6,
#         shape = 3,
#         color = "red")
#   }
#   plot_reactive(plt)
#   plt}
# )




## User proposed minimizer (demo)
# 
# 
# 
# observeEvent(input$game_click, {
#   user_point(round(input$game_click$x, 1))
# } )
# 
# 
# 
# output$plot_demo = renderPlot(plot_reactive() )
# output$plot_game = renderPlot(plot_reactive() )
# 
# #stats of the game
# 
# output$game_stats <- renderText(paste0(wins(), " wins out of ", plays(), " games."))
# output$game_level <- renderText(paste0("Level ", level_game(), ". ERM: ", choice() ))
