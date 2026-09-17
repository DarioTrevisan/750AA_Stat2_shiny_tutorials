ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Description",
      sidebarPanel(
        img(
          src = "pca_man-logo.jpg",
          alt = pca_man_logo_description,
          width = 400,
          height = 400
        )
      ),
      mainPanel(markdown(mds = pca_man_description))
    ),
    tabPanel(
      "Demo",
      sidebarPanel(
        radioButtons(
          inputId = "radio",
          label = "Input mode",
          choices = list("Draw points" = -1, "Draw line" = 0)
        ),
        actionButton("demo_erase_plot", "Erase plot", class = "btn-block"),
        checkboxInput(
          "demo_show_pacman",
          "Show directional standard deviation",
          FALSE
        ),
        checkboxInput("demo_show_ellipse", "Show normal data ellipse", FALSE),
        checkboxInput("demo_show_pc", "Show principal component 1 line", FALSE)
      ),
      mainPanel(
        textOutput("demo_info"),
        plotOutput("demo_plot", click = "demo_plot_click", dblclick = "demo_plot_dblclick")
      )
    ),
    tabPanel(
      "Play Ms PCA-Man!",
      textOutput("level_info"),
      plotOutput("game_plot", click = "game_plot_click", dblclick = "game_plot_dblclick"),
      actionButton("game_confirm_button", "Confirm your choice!", class = "btn-block"),
      actionButton("game_sample_points", "Sample a new dataset", class = "btn-block"),
      textOutput("game_hint"),
      textOutput("game_stats")
    ),
    tabPanel("Dataset description", mainPanel(uiOutput("documentation")))
  )
)
