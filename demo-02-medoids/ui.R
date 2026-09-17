



ui <- fluidPage(
  tabsetPanel(
    id = "tabs",
    tabPanel(
      "Description",
      sidebarPanel(
        img(
          src = "medoids-logo.jpg",
          alt = medoids_logo_description,
          width = 400,
          height = 400
        )
      ),
      mainPanel(markdown(mds = medoids_description))
    ),
    tabPanel(
      "Demo",
      sidebarPanel(
        markdown(mds = demo_medoids_description),
        actionButton("demo_erase_plot", "Draw empty plot", class = "btn-block"),
        radioButtons(
          "demo_radio",
          "Choose action for click on plot:",
          choices = c("Draw new point" = "demo_draw", "Select a point" = "demo_select"),
          selected = "demo_draw"
        ),
        actionButton("demo_show_medoid", "Show medoid", class = "btn-block"),
        actionButton("demo_remove_medoid", "Remove medoid", class = "btn-block"),
        textOutput("demo_info")
      ),
      mainPanel(
        plotOutput("demo_plot", click = "demo_plot_click", dblclick = "demo_plot_dblclick")
      )
    ),
    tabPanel(
      "Play Medoids!",
      textOutput("level_info"),
      textOutput("game_stats"),
      mainPanel(
        plotOutput("game_plot", click = "game_plot_click", dblclick = "game_plot_dblclick"),
        textOutput("game_info"),
        actionButton("game_confirm", "confirm/continue", class = "btn-block")
      )
    )
  )
)
