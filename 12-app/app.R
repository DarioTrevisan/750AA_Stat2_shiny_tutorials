library(shiny)
library(shinydashboard)

ui <- dashboardPage(
  dashboardHeader(title = "Basic dashboard"),

  dashboardSidebar(
    sidebarMenu(
      menuItem("Dashboard", tabName = "dashboard", icon = icon("dashboard")),
      menuItem("Widgets", tabName = "widgets", icon = icon("th"))
    )
  ),

  dashboardBody(
    tabsetPanel(
      id = "page",
      type = "hidden",

      # Your landing page
      tabPanelBody("landing-page",
        div(
          style = "position: absolute;
                   left: 0;
                   top: 0;
                   z-index: 10000;
                   width: 100%;
                   height: 100%;
                   background: lightblue;",
          div(
            style = "position: relative;
                     top: 30%;
                     left: 30%;",
            h1("Landing Page"),
            textInput("search", NULL),
            # Button to close landing page
            actionButton("close_landing_page", "Close")
          )
        )
      ),

      # Your content
      tabPanelBody("content",
        tabItems(
          # First tab content
          tabItem(tabName = "dashboard",
            fluidRow(
              box(plotOutput("plot1", height = 250)),

              box(
                title = "Controls",
                sliderInput("slider", "Number of observations:", 1, 100, 50)
              )
            )
          ),

          # Second tab content
          tabItem(tabName = "widgets",
            h2("Widgets tab content")
          )
        )
      )
    )
  )
)

server <- function(input, output, session) {
  set.seed(122)
  histdata <- rnorm(500)

  output$plot1 <- renderPlot({
    data <- histdata[seq_len(input$slider)]
    hist(data)
  })

  # Observe event to close landing page and open content
  observeEvent(input$close_landing_page, {
    updateTabsetPanel(session, "page", "content")
  })
}

shinyApp(ui, server)
