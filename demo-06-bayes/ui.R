





ui <- fluidPage(
  #titlePanel("Agry Bayes!"),
  tabsetPanel(
    id = "tabs",
    tabPanel("Description", sidebarPanel(
      img(
        src = "angry-bayes-logo.jpg",
        alt = game_logo_description,
        width = 400,
        height = 400
      )
    ), mainPanel(markdown(mds = game_description))),
    tabPanel(
      "Demo",
      sidebarPanel(width=3, position="left",
        markdown(mds ="### Prior probability
                 Click on the plot to define a prior probability (draw at least two points, do not add further points after application of Bayes rule)."),
        actionButton("demo_remove_last_point", "Remove last point (undo)", class="btn-block"),
       actionButton("demo_normalize", "Normalize to probability density", class = "btn-block"),
       checkboxInput("demo_show_points", "Show points", TRUE),
       checkboxInput("demo_show_prior", "Show prior curve", TRUE),
       checkboxInput("demo_show_mode", "Show mode (MAP)", FALSE),
       textOutput("demo_mode_text")
      ),
      mainPanel(width=6,
                #  verbatimTextOutput("demo_debug"),
                plotOutput("demo_plot", click  = "demo_plot_click", dblclick = "demo_plot_dblclick"),
                actionButton("demo_bayes", "Apply Bayes rule to update the prior!", class="btn-block"),
                actionButton("demo_erase_plot", "Erase plot and restart", class = "btn-block"),
      ),
      sidebarPanel(width=3, position="right",
        markdown(mds ="### Likelihood
                 Set the actual success probability and simulate independent experiments (coin tossings)."),
        sliderInput("demo_p", "Success probability:", min=0, max=1, value=0.5),
        sliderInput("demo_number_runs", "Number of tossings:", min=1, max=100, value=1),
        actionButton("demo_toss_coins", "Toss coins!", class="btn-block"),
        checkboxInput("demo_show_outcomes", "Show outcomes", TRUE),
        textOutput("demo_outcomes"),
        checkboxInput("demo_show_likelihood", "Show likelihood", TRUE),
        checkboxInput("demo_show_posterior", "Show posterior", FALSE),
      )
    ),
    tabPanel("Play Angry Bayes!", sidebarPanel(
      width = 3,
      position = "right",
      #verbatimTextOutput("game_debug"),
      actionButton("game_erase_plot", "Erase plot", class="btn-block"),
      hr(),
          markdown(mds ="Click on the plot to define a prior probability."),
          actionButton("game_remove_last_point", "Remove last point (undo)", class="btn-block"),
      
          textOutput("game_prior_hint"),
      hr(),
      textOutput("game_coins_info"),
         actionButton("game_bayes", "Toss a coin!", class="btn-block"),
      hr(),
         textOutput("game_map"),
          actionButton("game_confirm", "Confirm your estimate", class = "btn-block"),
        textOutput("game_level_outcome")
       # actionButton("game_next_level", "Move to next level", class = "btn-block"),
    ),
        mainPanel(width=6,
                  #verbatimTextOutput("game_debug"),
                  textOutput("game_level_info"),
                  plotOutput("game_plot", click  = "game_plot_click", dblclick = "game_plot_dblclick"),
                  textOutput("game_outcomes"),
                  plotOutput("game_plot_paths"),
                  
        ),
    sidebarPanel(width=3, position="right",
                 plotOutput("game_plot_path") 
    )
    ))
  )
  