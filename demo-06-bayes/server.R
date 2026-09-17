

server <- function(input, output, session) {
  
#output$data_description <- renderText( tools:::Rd2txt(utils:::.getHelpFile(as.character(help("iris", help_type="text")))))

 source("server_demo.R", local = TRUE)
  source("server_game.R", local = TRUE)
  
  
  
}
