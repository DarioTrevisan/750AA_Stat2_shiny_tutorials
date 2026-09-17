server <- function(input, output) {

  # ---- reactive state -------------------------------------------------

  # collect the drawn points and their cluster assignment
  values <- reactiveValues(
    data_points = data.frame(x = numeric(), y = numeric(), clusters = numeric()),
    sil = data.frame(cluster = numeric(), neighbor = numeric(), sil_width = numeric())
  )

  # stores the agnes/diana tree for the HC methods (NULL when there is none)
  hc_tree <- reactiveVal(NULL)

  # keeps the last chosen number of clusters so the slider value survives
  # being re-created (its max changes whenever the number of points changes)
  num_clusters <- reactiveVal(1)

  # clears every output that depends on a previous clustering computation;
  # called whenever the point set changes (add / remove / erase) so the
  # displayed indicators and plots never go stale
  reset_cluster_outputs <- function() {
    hc_tree(NULL)
    values$sil <- data.frame(cluster = numeric(), neighbor = numeric(), sil_width = numeric())
    output$error <- renderText("")
    render_silhouette_outputs()
    render_dendrogram_output()
  }

  # ---- Adventurer's Workshop tab ---------------------------------------

  output$debug <- renderText(input$upload$datapath)

  output$quest_info <- renderText(input$quest)

  # add a point on click
  observeEvent(input$plot_draw_click, {
    add_row <- data.frame(
      x = input$plot_draw_click$x,
      y = input$plot_draw_click$y,
      clusters = 1
    )
    values$data_points <- rbind(values$data_points, add_row)
    reset_cluster_outputs()
  })

  observeEvent(input$remove_point, {
    req(nrow(values$data_points) > 0)
    values$data_points <- values$data_points[-nrow(values$data_points), ]
    reset_cluster_outputs()
  })

  observeEvent(input$remove_all, {
    values$data_points <- data.frame(x = numeric(), y = numeric(), clusters = numeric())
    reset_cluster_outputs()
  })

  # load a previously saved dataset
  observe({
    req(input$upload)
    values$data_points <- read.table(
      input$upload$datapath,
      header = TRUE,
      skip = 2
    )
    reset_cluster_outputs()
  })

  # save the current dataset (and its cluster assignment, if any)
  output$download <- downloadHandler(
    filename = function() {
      paste0("FC_points_quest-", names(quest_list[quest_list == input$quest]), ".csv")
    },
    content = function(file) {
      write(paste("#", names(quest_list[quest_list == input$quest])), file)
      write(paste("#", input$quest), file, append = TRUE)
      write.table(values$data_points, file, append = TRUE, row.names = FALSE)
    }
  )

  # the drawing canvas: one labeled point per row, no cluster coloring yet
  output$plot_draw_points <- renderPlot({
    ggplot(values$data_points, aes(x = x, y = y, label = rownames(values$data_points))) +
      geom_label(size = 5, fill = "black", colour = "white", fontface = "bold") +
      lims(x = c(-1, 1), y = c(-1, 1)) +
      theme(aspect.ratio = 1)
  })

  # ---- Clusterize! tab --------------------------------------------------

  # the number-of-clusters slider is rebuilt whenever the point count
  # changes (its max depends on it), so we remember the user's last choice
  # in num_clusters() to restore it across rebuilds
  output$cluster_number_ui <- renderUI({
    max_clusters <- max(1, min(10, nrow(values$data_points)))
    sliderInput(
      "cluster_number",
      "Number of clusters:",
      min = 1,
      max = max_clusters,
      value = min(max(1, num_clusters()), max_clusters),
      step = 1
    )
  })

  observe({
    req(input$cluster_number)
    num_clusters(input$cluster_number)
  })

  output$cluster_choice_description <- renderText(paste(
    get_clustering_description(input$cluster_choice),
    "(Help tab for more information)"
  ))

  # the cluster plot: one labeled point per row, colored by cluster
  output$plot_cluster_points <- renderPlot({
    ggplot(
      values$data_points,
      aes(x = x, y = y, label = rownames(values$data_points), fill = factor(clusters))
    ) +
      geom_label(size = 5, colour = "white", fontface = "bold") +
      lims(x = c(-1, 1), y = c(-1, 1)) +
      theme(aspect.ratio = 1)
  })

  # renders the silhouette boxplot (and the wcss/dunn/avg-silhouette text)
  # from the current values$sil; called after every successful clustering
  # and whenever the cluster set is cleared
  render_silhouette_outputs <- function() {
    if (nrow(values$sil) != 0) {
      output$wcss <- renderText(paste0(
        "Within-Cluster-Sum-of-Squares: ", round(wcss(values$data_points), 2)
      ))
      output$dunn <- renderText(paste0(
        "Dunn Index: ", round(dunn_index(values$data_points), 2)
      ))
      output$silhouette_average <- renderText(paste0(
        "Average silhouette: ", round(mean(values$sil$sil_width), 2)
      ))
      output$plot_silhouette_boxplots <- renderPlot(
        ggplot(
          values$sil,
          aes(x = factor(cluster), y = sil_width, group = factor(cluster), fill = factor(cluster))
        ) +
          geom_boxplot() +
          ggtitle("Cluster silhouettes") +
          labs(x = "Cluster", y = "Silhouette width") +
          theme(legend.position = "none")
      )
    } else {
      output$wcss <- renderText("")
      output$dunn <- renderText("")
      output$silhouette_average <- renderText("")
      output$plot_silhouette_boxplots <- renderPlot(
        ggplot(
          values$sil,
          aes(x = factor(cluster), y = sil_width, group = factor(cluster), fill = factor(cluster))
        ) +
          labs(x = "Cluster", y = "Silhouette width")
      )
    }
  }

  # renders the dendrogram for the tree-based methods (single/average/
  # complete linkage and diana); clears the plot for the other methods so
  # a dendrogram from a previous method never lingers on screen
  render_dendrogram_output <- function() {
    if (nrow(values$sil) != 0 && input$cluster_choice %in% algos_with_dendro) {
      k <- input$cluster_number
      clust <- cutree(hc_tree(), k)
      dendr <- dendro_data(hc_tree(), type = "rectangle") # convert for ggplot
      clust.df <- data.frame(label = rownames(values$data_points), cluster = factor(clust))
      dendr[["labels"]] <- merge(dendr[["labels"]], clust.df, by = "label")
      rect <- aggregate(x ~ cluster, label(dendr), range)
      rect <- data.frame(rect$cluster, rect$x)
      ymax <- mean(hc_tree()$height[length(hc_tree()$height) - ((k - 2):(k - 1))])

      plt <- ggplot() +
        geom_segment(data = segment(dendr), aes(x = x, y = y, xend = xend, yend = yend)) +
        geom_label(
          data = label(dendr),
          aes(x, y, label = label, hjust = 0, fill = cluster),
          size = 5,
          colour = "white",
          fontface = "bold"
        ) +
        geom_rect(
          data = rect,
          aes(xmin = X1 - 0.1, xmax = X2 + 0.1, ymin = 0.03, ymax = ymax, color = rect.cluster, fill = rect.cluster),
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
    } else {
      output$dendro_info <- renderText("")
      output$plot_dendrogram <- renderPlot(NULL)
    }
  }

  # compute the clustering for the current method / point set
  observeEvent(input$compute_cluster, {
    req(input$cluster_number)

    if (input$cluster_number > nrow(values$data_points)) {
      output$error <- renderText("ERROR: the desired number of clusters is larger than the number of points!")
      return(invisible(NULL))
    }

    result <- tryCatch({
      if (input$cluster_choice == "kmeans") {
        values$data_points$clusters <- kmeans(values$data_points[, 1:2], input$cluster_number)$cluster
      } else if (input$cluster_choice == "pam") {
        values$data_points$clusters <- pam(
          values$data_points[, 1:2],
          input$cluster_number,
          cluster.only = TRUE
        )
      } else if (input$cluster_choice == "pam_manhattan") {
        values$data_points$clusters <- pam(
          values$data_points[, 1:2],
          input$cluster_number,
          metric = "manhattan",
          cluster.only = TRUE
        )
      } else if (input$cluster_choice == "diana") {
        hc_tree(diana(values$data_points[, 1:2]))
        values$data_points$clusters <- cutree(hc_tree(), input$cluster_number)
      } else {
        # single / average / complete linkage, computed via agnes
        hc_tree(agnes(values$data_points[, 1:2], method = input$cluster_choice))
        values$data_points$clusters <- cutree(hc_tree(), input$cluster_number)
      }
      values$sil <- data.frame(silhouette(
        values$data_points$clusters,
        dist(values$data_points[, 1:2])
      ))
      TRUE
    }, error = function(e) {
      conditionMessage(e)
    })

    if (isTRUE(result)) {
      output$error <- renderText("")
    } else {
      output$error <- renderText(paste("ERROR:", result))
      values$sil <- data.frame(cluster = numeric(), neighbor = numeric(), sil_width = numeric())
    }

    render_silhouette_outputs()
    render_dendrogram_output()
  })

  # ---- Help tab -----------------------------------------------------------

  tmp <- tempfile()
  output$documentation <- renderUI({
    req(input$cluster_choice)
    pack <- if (input$cluster_choice == "kmeans") "stats" else "cluster"
    rdfile <- paste0(get_clustering_function(input$cluster_choice), ".Rd")
    req(rdfile %in% names(Rd_db(pack)))
    Rd2HTML(Rd_db(pack)[[rdfile]], tmp, no_links = TRUE, package = pack)
    includeHTML(tmp)
  })

  # bind the indicator/plot outputs to a blank initial state
  reset_cluster_outputs()
}
