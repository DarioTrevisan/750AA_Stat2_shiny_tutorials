


ui <- fluidPage(
  #titlePanel("Win by Empirical Risk Minimization 1D"),
  tabsetPanel(id="tabs",
              tabPanel("Description", sidebarPanel(
                img(
                  src = "werms-logo.jpg",
                  alt = werms_logo_description,
                  width = 400,
                  height = 400
                )
              ),
              mainPanel(markdown(mds = werms_description), )),
    tabPanel(
      "Demo",
      sidebarPanel(
        selectInput(
          "demo_loss_choice",
          "Choose the cost function:",
          c(
            "Ordinary least squares (OLS)" = "OLS" ,
            "Absolute value" = "ABS",
            "Huber (delta = 1)" = "HUB",
            "First quartile" = "1QUART",
            "First decile" = "10PERC",
            "Exponential of absolute value" = "EXP"
          )
        ),
        textOutput("demo_choice_description"),
        sliderInput("demo_num_points", "Choose sample size", min=1, max=20, value =
                       5),
        #actionButton("demo_button_sample", "Sample points", class = "btn-block"),
        checkboxInput("demo_show_loss", "Show empirical loss", FALSE),
        checkboxInput("demo_show_minimizer", "Show minimizer", FALSE),
        textOutput("demo_proposed_minimizer"),
        textOutput("demo_minimizer_description"),
      ),
      mainPanel(
        plotOutput("demo_plot", click = "demo_click")
      )
    ),
    tabPanel("Play WERMS!",
             mainPanel(textOutput("game_level"),
               textOutput("game_level_info"),
               #actionButton("game_button_play", "Play!", class = "btn-block"),
               plotOutput("game_plot", click = "game_click"),
               textOutput("game_proposed_minimizer"),
               textOutput("game_minimizer_description"),
               textOutput("game_stats")
             )
             )
  )
)
