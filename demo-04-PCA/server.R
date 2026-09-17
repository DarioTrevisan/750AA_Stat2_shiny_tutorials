server <- function(input, output, session) {

  source("server_demo.R", local = TRUE)
  source("server_game.R", local = TRUE)

  # dataset description tab: render the R help page for the currently sampled dataset
  tmp <- tempfile()
  output$documentation <- renderUI({
    rdfile <- paste0(game_name_dataset(), ".Rd")
    req(rdfile %in% names(Rd_db("datasets")))
    Rd2HTML(Rd_db("datasets")[[rdfile]], tmp, no_links = TRUE, package = "datasets")
    includeHTML(tmp)
  })
}
