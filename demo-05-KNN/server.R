


server <- function(input, output, session) {
  source("server_demo.R", local = TRUE)
  source("server_game.R", local = TRUE)
  
  
  tmp <- tempfile()
  output$documentation <- renderUI({
    rdfile <- paste0(game$name_dataset, ".Rd")
    req(rdfile %in% names(Rd_db("MedDataSets")))
    Rd2HTML(Rd_db("MedDataSets")[[rdfile]],
            tmp,
            no_links = TRUE,
            package = "MedDataSets")
    includeHTML(tmp)
  })
  
}
