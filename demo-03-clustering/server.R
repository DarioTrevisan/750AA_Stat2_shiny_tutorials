


server <- function(input, output) {

num_clusters <- reactiveVal(1)
  
output$cluster_number_ui <- renderUI({
    sliderInput(
      "cluster_number",
      "Number of clusters:",
      min = 1,
      max = max(1, min(10, length(values$data_points[,1])-1)),
      value = max(1, num_clusters()),
      step = 1
    )
  })

observe( num_clusters(input$cluster_number))

  #sliderInput(
  #  "cluster_number",
  #  "Number of clusters:",
  #  min = 1,
  #  max = 10,
  #  value = 2
  #),
  
  
  # outputs
  
  output$debug <- renderPrint(input$quest)
  
  # file upload and download functions
  
  output$debug <-  renderText(input$upload$datapath)
  
  
  observe({
    req(input$upload)
    values$data_points <- read.table(input$upload$datapath,
                                     header = TRUE,
                                     skip = 2)
  })
  
  output$quest_info <- renderText(input$quest)
  
  output$cluster_choice_description <- renderText(paste(
    get_clustering_description(input$cluster_choice),
    "(Help tab for more information)"
  ))
  
  output$download <- downloadHandler(
    filename = function() {
      paste0("FC_points_quest-", names(quest_list[quest_list == input$quest]), ".csv")
    },
    content = function(file) {
      write(paste("#", names(quest_list[quest_list == input$quest])), file)
      write(paste("#", input$quest), file, append = TRUE)
      write.table(values$data_points,
                  file,
                  append = TRUE,
                  row.names = FALSE)
    }
  )
  # collect reactive values here
  
  values <- reactiveValues()
  
  
  # store the dendrogram for HC methods
  
  hc_tree <- reactiveVal()
  
  # define the data frame to collect the sample points
  
  values$data_points <- data.frame(x = numeric(),
                                   y = numeric(),
                                   clusters = numeric())
  
  values$sil <- data.frame(cluster = numeric(),
                           neigbor = numeric(),
                           sil_width = numeric())
  
  # define a reactive val to store the dendrogram (so that k can vary easily)
  
  
  # at click add the points
  
  observeEvent(input$plot_draw_click, {
    add_row <- data.frame(
      x = input$plot_draw_click$x,
      y = input$plot_draw_click$y,
      clusters = 1
    )
    # add row to the data.frame
    values$data_points <- rbind(values$data_points, add_row)
    # empty past cluster variables
    hc_tree()
    values$sil <- data.frame(cluster = numeric(),
                             neigbor = numeric(),
                             sil_width = numeric())
  })
  
  
  # define the drawing plot
  
  
  output$plot_draw_points = renderPlot({
    ggplot(values$data_points, aes(
      x = x,
      y = y,
      label = rownames(values$data_points)
    )) +
      geom_label(
        size = 5,
        fill = "black",
        colour = "white",
        fontface = "bold"
      ) +
      lims(x = c(-1, 1), y = c(-1, 1)) +
      theme(aspect.ratio = 1)
  })
  
  # Compute the species
  
  
  
  # define the 'cluster' plot
  
  output$plot_cluster_points = renderPlot({
    ggplot(values$data_points,
           aes(
             x = x,
             y = y,
             label = rownames(values$data_points),
             fill = factor(clusters)
           )) +
      geom_label(size = 5,
                 colour = "white",
                 fontface = "bold") +
      lims(x = c(-1, 1), y = c(-1, 1)) +
      theme(aspect.ratio = 1)
  })
  
  # define the 'silhouette' plot
  
  output$plot_silhouette_boxplots <- renderPlot(
    ggplot(
      values$sil,
      mapping = aes(
        y = sil_width,
        x = factor(cluster),
        group =
          factor(cluster),
        fill = factor(cluster)
      )
    ) +
      geom_boxplot() +
      labs(y = "cluster", x = "silhouette") +
      theme(legend.position =
              "none")
  )
  
  
  # define the dendrogram plot
  
  compute_dendrogram <- reactive({
    if (nrow(values$sil) != 0 &&
        input$cluster_choice %in% algos_with_dendro) {
      k <- input$cluster_number
      clust <- cutree(hc_tree(), k)
      dendr    <- dendro_data(hc_tree(), type = "rectangle") # convert for ggplot
      clust.df <- data.frame(label = rownames(values$data_points),
                             cluster = factor(clust))
      dendr[["labels"]]   <- merge(dendr[["labels"]], clust.df, by = "label")
      rect <- aggregate(x ~ cluster, label(dendr), range)
      rect <- data.frame(rect$cluster, rect$x)
      ymax <- mean(hc_tree()$height[length(hc_tree()$height) - ((k - 2):(k -
                                                                           1))])
      
      plt <- ggplot() +
        geom_segment(data = segment(dendr), aes(
          x = x,
          y = y,
          xend = xend,
          yend = yend
        )) +
        geom_label(
          data = label(dendr),
          aes(
            x,
            y,
            label = label,
            hjust = 0,
            fill = cluster
          ),
          size = 5,
          colour = "white",
          fontface = "bold"
        ) +
        geom_rect(
          data = rect,
          aes(
            xmin = X1 - 0.1,
            xmax = X2 + 0.1,
            ymin = 0.03,
            ymax = ymax,
            color = rect.cluster,
            fill = rect.cluster
          ),
          alpha = 0.2
        ) +
        coord_flip() +
        scale_y_reverse(expand = c(0.2, 0)) +
        theme(
          axis.title = element_blank(),
          axis.ticks = element_blank(),
          axis.text.y = element_blank(),
          legend.position = "none"
        )
      output$dendro_info <- renderText("Computed dendrogram:")
      output$plot_dendrogram <- renderPlot(plt)
    }
  })
  
  # button actions
  
  observeEvent(input$remove_point, {
    rem_row <- values$data_points[-nrow(values$data_points), ]
    values$data_points <- rem_row
    # empty past cluster variables
    hc_tree()
    values$sil <- data.frame(cluster = numeric(),
                             neigbor = numeric(),
                             sil_width = numeric())
  })
  
  observeEvent(input$remove_all, {
    values$data_points <- data.frame(x = numeric(),
                                     y = numeric(),
                                     clusters = factor())
    # empty past cluster variables
    hc_tree()
    values$sil <- data.frame(cluster = numeric(),
                             neigbor = numeric(),
                             sil_width = numeric())
  })
  
  
  
  compute_silhouette <- reactive({
    if (nrow(values$sil) != 0) {
      output$wcss <- renderText(paste0(
        "Within-Cluster-Sum-of-Squares: ",
        round(wcss(values$data_points), 2)))
      output$dunn <- renderText(paste0(
        "Dunn Index: ",
        round(dunn_index(values$data_points), 2)))
      output$silhouette_average <- renderText(paste0(
        "Average silhouette: ",
        round(mean(values$sil$sil_width), 2)
      ))
      output$plot_silhouette_boxplots <- renderPlot(
        ggplot(
          values$sil,
          mapping = aes(
            x = sil_width,
            y = factor(cluster),
            group =
              factor(cluster),
            fill = factor(cluster)
          )
        ) +
          
          geom_boxplot() +
          ggtitle("Cluster silhouettes")+
          labs(y = "cluster", x = "silhouette") +
          theme(legend.position =
                  "none")
      )
    }
    else {
      output$silhouette_average <- renderText("")
      output$plot_silhouette_boxplots <- renderPlot(ggplot(
        values$sil,
        mapping = aes(
          x = sil_width,
          y = factor(cluster),
          group =
            factor(cluster),
          fill = factor(cluster)
        )
      ) +
        labs(x = "cluster", y = "silhouette"))
    }
  })
  
  
  
  
  
  # compute the cluster
  
  observeEvent(input$compute_cluster, {
    if (input$cluster_number > nrow(values$data_points)) {
      output$error <- renderText("ERROR: the desired number of clusters is larger than the number of points!")
    }
    else {
      if (input$cluster_choice == "kmeans") {
        values$data_points$clusters <- kmeans(values$data_points[, 1:2], input$cluster_number)$cluster
      }
      else if (input$cluster_choice == "pam") {
        values$data_points$clusters <- pam(values$data_points[, 1:2],
                                           input$cluster_number,
                                           cluster.only = TRUE)
      }
      else if (input$cluster_choice == "pam_manhattan") {
        values$data_points$clusters <- pam(
          values$data_points[, 1:2],
          input$cluster_number,
          metric = "manhattan",
          cluster.only = TRUE
        )
      }
      else if (input$cluster_choice == "diana") {
        # compute the dendrogram
        hc_tree(diana(values$data_points[, 1:2]))
        # get the clusters
        values$data_points$clusters <- cutree(hc_tree(), input$cluster_number)
      }
      else {
        # compute the dendrogram with method specified by the name
        hc_tree(agnes(values$data_points[, 1:2], method = input$cluster_choice))
        # uses cutree to get the clusters
        values$data_points$clusters <- cutree(hc_tree(), input$cluster_number)
      }
      #computes the silhouette
      values$sil <- data.frame(silhouette(
        values$data_points$clusters,
        dist(values$data_points[, 1:2])
      ))
      compute_silhouette()
      # computes the dendrogram
      compute_dendrogram()
    }
  })
  
  # code for the help tab
  
  tmp <- tempfile()
  output$documentation <- renderUI({
    if (input$cluster_choice == "kmeans") {
      pack <- "stats"
    }
    else
    {
      pack <- "cluster"
    }
    rdfile <- paste0(get_clustering_function(input$cluster_choice), ".Rd")
    req(rdfile %in% names(Rd_db(pack)))
    Rd2HTML(Rd_db(pack)[[rdfile]],
            tmp,
            no_links = TRUE,
            package = pack)
    includeHTML(tmp)
  })
  
  
}
