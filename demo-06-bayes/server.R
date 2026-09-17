

server <- function(input, output, session) {
  # server_demo.R and server_game.R are sourced into this same local
  # environment (local = TRUE), so top-level variable/reactive names must be
  # kept distinct between the two files to avoid one tab silently overwriting
  # the other's state (e.g. use demo_* / game_* prefixes for anything that
  # isn't already namespaced inside the demo/game reactiveValues).
  source("server_demo.R", local = TRUE)
  source("server_game.R", local = TRUE)
}
