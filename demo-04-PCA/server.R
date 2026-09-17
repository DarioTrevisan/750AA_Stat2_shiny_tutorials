

server <- function(input, output, session) {
  
#output$data_description <- renderText( tools:::Rd2txt(utils:::.getHelpFile(as.character(help("iris", help_type="text")))))

 source("server_demo.R", local = TRUE)
  source("server_game.R", local = TRUE)
  
  
  
  
  # code for the tab with data set description from R help
  
  tmp <- tempfile()
  output$documentation <- renderUI({
    rdfile <- paste0(game_name_dataset(), ".Rd")
    req(rdfile %in% names(Rd_db("datasets")))
    Rd2HTML(Rd_db("datasets")[[rdfile]], tmp, no_links = TRUE, package="datasets")
    includeHTML(tmp)
  })
  
}
