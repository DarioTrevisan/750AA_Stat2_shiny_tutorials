


ui <- fluidPage(
  #titlePanel("Clustering"),
  tabsetPanel(
    tabPanel("Description", sidebarPanel(
      img(
        src = "final-clustering-logo.jpg",
        alt = final_clustering_logo_description,
        width = 400,
        height = 400
      )
    ), mainPanel(
      markdown(mds = final_clustering_description)
    )),
    tabPanel(
      "Adventurer's Workshop",
      verbatimTextOutput("debug"),
      sidebarPanel(
        width = 3,
        selectInput("quest", "Choose your quest:", quest_list),
        
        textOutput("quest_info")
      ),
      mainPanel(
        width = 6,
        plotOutput("plot_draw_points", click = "plot_draw_click")
      ),
      sidebarPanel(
        width = 3,
        position = "right",
        actionButton("remove_all", "Erase the board", class = "btn-block"),
        actionButton("remove_point", "Remove last point", class = "btn-block"),
        downloadButton("download", "Save points (and cluster)", class = "btn-block"),
        fileInput("upload", "Load points", accept = ".csv")
      ),
      
    ),
    tabPanel(
      "Clusterize!",
      sidebarPanel(
        position = "left",
        width = 3,
        selectInput("cluster_choice", "Clustering method:", cluster_algos),
        textOutput("cluster_choice_description"),
        uiOutput("cluster_number_ui"),
        #sliderInput(
        #  "cluster_number",
        #  "Number of clusters:",
        #  min = 1,
        #  max = 10,
        #  value = 2
        #),
        actionButton("compute_cluster", "Compute the cluster!", class = "btn-block"),
        #checkboxInput("show_silhouette", "Show silouhettes (boxplots)."),
        #  checkboxInput("show_dendrogram", "Show dendrogram (for HC methods)."),
        
      ),
      mainPanel(
        width = 6,
        textOutput("error"),
        plotOutput("plot_cluster_points")
      ),
      sidebarPanel(
        width = 3,
        position = "right",
        markdown(mds = "### Indicators"),
        textOutput("wcss"),
        textOutput("dunn"),
        textOutput("silhouette_average"),
        plotOutput("plot_silhouette_boxplots"),
        hr(),
        textOutput("dendro_info"),
        plotOutput("plot_dendrogram"),
      )
    ),
    tabPanel("Help", mainPanel(uiOutput("documentation")))
  )
)
