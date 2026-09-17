


ui <- fluidPage(
  #titlePanel("Hello Neighbor! (k-nearest)"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Description", sidebarPanel(
      img(
        src = "hello-knn.jpg",
        alt = game_logo_description,
        width = 400,
        height = 400
      )
    ), mainPanel(markdown(mds = game_description))),
    tabPanel(
      "Demo",
      sidebarPanel(
        width = 3,
        position = "left",
        radioButtons("demo_radio", "Choose action for click on plot:", choices = c("Draw new training point" = "demo_train", "Draw a test point" = "demo_test"), selected="demo_train"),
        textOutput("demo_choice_click"),
        hr(),
        sliderInput(
          "demo_class",
          "Choose point class",
          min = 1,
          max = 10,
          value = 1,
          step = 1
        ),
        #markdown(mds = "Click on the plot to add (training) points of a chosen class."),
        hr(),
       # markdown(mds = "### Add test points
       #        Double click on the plot to add (test) points to be classified."),
        tableOutput("demo_test_points"),
        hr(),
        uiOutput("demo_slider_choose_k"),
        actionButton("demo_classify", "Classify!", class = "btn-block")
      ),
      mainPanel(
        width = 6,
        plotOutput("demo_plot", click  = "demo_plot_click", dblclick = "demo_plot_dblclick"),
        actionButton("demo_erase_plot", "Erase data set", class = "btn-block"),
        actionButton("demo_remove_test_points", "Erase test points", class =
                       "btn-block"),
        
      ),
      sidebarPanel(
        width = 3,
        position = "right",
        checkboxInput("demo_show_error", "Show training and cross-validation (cv) errors"),
        uiOutput("demo_slider_zoom_k"),
        plotOutput("demo_error")
      )
    ),
    tabPanel(
      "Play!",
      sidebarPanel(
        width = 3,
        position = "right",
        verbatimTextOutput("game_debug"),
        tableOutput("game_debug_table"),
        textOutput("game_hint"),
        hr(),
        tableOutput("game_test_points_table"),
        
        hr(),
        uiOutput("game_radio_choose_class")
        
      ),
      mainPanel(
        width = 6,
        textOutput("game_over"),
        textOutput("game_info"),
        plotOutput("game_plot", click  = "game_plot_click", dblclick = "game_plot_dblclick"),
        textOutput("game_points_info"),
      ),
      sidebarPanel(
        width = 3,
        position = "right",
        p("Error rates:"),
        plotOutput("game_error")
      )
    ),
    tabPanel("Dataset description", mainPanel(uiOutput("documentation")))
  )
)
