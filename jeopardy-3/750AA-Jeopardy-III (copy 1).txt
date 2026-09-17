library(shiny)

# team names

red_name <- "Gestionali"
blue_name <- "Resto del Mondo"

# main title

titolo <- paste("750AA Statistica II: Jeopardy! Parte 3: Serie Storiche - ", red_name, "vs", blue_name)

# --- Placeholder Questions and Answers ---
categories <- c("Decomposizione", "ModelliSemplici", "Stime", "ETS", "ARIMA")

questions <- list(
  Decomposizione = list(
    q1 = "Cos’è la decomposizione di una serie storica?",
    q2 = "Qual è la differenza tra decomposizione additiva e moltiplicativa?",
    q3 = "Cosa rappresenta la componente di stagionalità?",
    q4 = "Perché si usa una media mobile nella decomposizione classica?",
    q5 = "Quando può fallire la decomposizione classica?"
  ),
  ModelliSemplici = list(
    q1 = "Cosa prevede il modello naive?",
    q2 = "Come funziona il modello seasonal naive?",
    q3 = "Come viene applicato il KNN alle serie storiche?",
    q4 = "In quali situazioni il naive è sorprendentemente competitivo?",
    q5 = "Qual è un limite dell’approccio KNN nelle serie storiche?"
  ),
  Stime = list(
    q1 = "Cos’è un intervallo di predizione?",
    q2 = "Perché un intervallo di predizione è più largo di un intervallo di confidenza?",
    q3 = "Come si usa il bootstrap dei residui per costruire intervalli?",
    q4 = "Qual è un problema del bootstrap naive su tutti i dati temporali?",
    q5 = "Sotto quali ipotesi gli intervalli parametrici sono affidabili?"
    ),
  ETS = list(
    q1 = "Cosa significa ETS in un modello di serie storica?",
    q2 = "Che differenza c’è tra trend additivo e moltiplicativo negli ETS?",
    q3 = "Qual è il ruolo della componente ‘errore’ in ETS?", 
    q4 = "Qual è il ruolo del damping per il trend negli ETS?",
    q5 = "Qual è un vantaggio degli ETS rispetto agli ARIMA?"
  ),
  ARIMA = list(
    q1 = "Cosa significano le sigle AR, MA e ARIMA?",
    q2 = "A cosa serve la differenziazione nei modelli ARIMA?",
    q3 = "Come si riconosce un modello AR dal suo grafico ACF?",
    q4 = "Perché i modelli SARIMA includono stagionalità?",
    q5 = "Qual è un limite dei modelli ARIMA?"
    )
)

answers <- list(
  Decomposizione = list(
    q1 = "È la scomposizione in trend, stagionalità e componente residua.",
    q2 = "Additiva: componenti si sommano. Moltiplicativa: trend e stagionalità variano in proporzione al livello.",
    q3 = "La variazione periodica che si ripete con cadenza regolare, es. settimanale o annuale.",
    q4 = "Per stimare il trend eliminando oscillazioni a breve termine.",
    q5 = "Quando la stagionalità non è stabile o il trend è troppo irregolare."
  ),
  ModelliSemplici = list(
    q1 = "Che il valore futuro sia uguale all’ultima osservazione disponibile.",
    q2 = "Ripete l’osservazione della stessa posizione nella stagione precedente.",
    q3 = "Cerca pattern simili nella serie passata e usa le relative evoluzioni per prevedere il futuro.",
    q4   = "In serie molto rumorose o senza trend/stagionalità stabile.",
    q5   = "La difficoltà nel definire una distanza significativa tra finestre temporali o selezionare gli iperparametri di lag e k"
  ),
  Stime = list(
    q1 = "Un intervallo che contiene una futura osservazione con una certa probabilità.", 
    q2 = "Perché include anche la variabilità dei futuri errori di previsione.",
    q3 =  "Ricampionando i residui o segmenti della serie per generare molte traiettorie future.",
    q4 = "Ignora la dipendenza temporale e rompe la struttura della serie.",
    q5 = "Se i residui sono non-correlati (i.i.d.) e gaussiani con varianza costante."  
    ),
  ETS = list(
    q1 = "Errore, Trend, Stagionalità: tre componenti modellate esplicitamente.",
    q2 = "Additivo: crescita lineare. Moltiplicativo: crescita proporzionale al livello (esponenziale).",
    q3 = "Modellizza il residuo tra la previsione e l'osservazione.",
    q4 = "Evita di sovrastimare la crescita, impone convergenza per orizzonti lontani",
    q5 = "Gestiscono naturalmente trend e stagionalità non lineari o variabili nel tempo."
  ),
  ARIMA = list(
    q1 = "AutoRegressivo, Media Mobile e la loro combinazione con Integrazione.",
    q2 = "A rendere la serie stazionaria, eliminando trend.",
    q3 = "L’ACF decresce esponenzialmente",
    q4 = "Aggiungono componenti AR, MA e differenze a lags stagionali (es. 12, 24).",
    q5 = "Faticano con stagionalità non stabile o con cambiamenti strutturali improvvisi."
  )
)

# --- UI ---
ui <- fluidPage(
  titlePanel(paste("🎯", titolo)),
  
  uiOutput("mainUI")
)

# --- SERVER ---
server <- function(input, output, session) {
  
  current_question <- reactiveVal(NULL)
  show_answer <- reactiveVal(FALSE)
  game_over <- reactiveVal(FALSE)
  
  scores <- reactiveValues(red = 0, blue = 0)
  answered <- reactiveValues()
  
  # Initialize all questions as unanswered
  for (cat in categories) {
    for (i in 1:5) {
      answered[[paste0(cat, "_", i)]] <- FALSE
    }
  }
  
  # Check if game is over (all questions answered)
  checkGameOver <- function() {
    all(sapply(names(answered), function(x) answered[[x]]))
  }
  
  # --- MAIN UI rendering ---
  output$mainUI <- renderUI({
    if (game_over()) {
      # Victory screen
      winner <- if (scores$red > scores$blue) {
        paste("🔴 Vince", red_name, "!")
      } else if (scores$blue > scores$red) {
        paste("🔵 Vince", blue_name, "!")
      } else {
        "🤝 Pareggio!"
      }
      
      tagList(
        br(), br(),
        h1("🏆 Game Over!", align = "center"),
        h2(winner, align = "center"),
        br(),
        fluidRow(
          column(6, h2(paste("🔴", red_name, ":", scores$red), align = "center", style = "color:red;")),
          column(6, h2(paste("🔵", blue_name, ":", scores$blue), align = "center", style = "color:blue;"))
        ),
        br(),
        div(align = "center",
            actionButton("restart", "Ricomincia il gioco", class = "btn-success btn-lg"))
      )
    } else {
      # Game board screen
      
      tagList(
        fluidRow( 
          uiOutput("qaDisplay"),
          column(6, h3(paste("🔴", red_name), align = "center", style = "color:red;"),
                 textOutput("scoreRed", container = h2, inline = TRUE)),
          column(6, h3(paste("🔵", blue_name), align = "center", style = "color:blue;"),
                 textOutput("scoreBlue", container = h2, inline = TRUE))
        ),
        
        hr(),
        fluidRow(
          lapply(categories, function(cat) {
            column(2,
                   tags$h4(cat, align = "center"),
                   lapply(1:5, function(i) {
                     qid <- paste0(cat, "_", i)
                     actionButton(
                       inputId = qid,
                       label = if (answered[[qid]]) "—" else paste0(i * 100),
                       width = "100%",
                       style = paste0(
                         "margin-bottom:10px; height:60px; font-size:20px;",
                         if (answered[[qid]]) "background-color:lightgray;"
                       )
                     )
                   })
            )
          })
        )
       
      )
    }
  })
  
  # --- Observe question clicks ---
  observe({
    for (cat in categories) {
      for (i in 1:5) {
        local({
          ccat <- cat
          ii <- i
          observeEvent(input[[paste0(ccat, "_", ii)]], {
            qid <- paste0(ccat, "_", ii)
            if (!answered[[qid]] && !game_over()) {
              current_question(list(category = ccat, index = ii))
              show_answer(FALSE)
            }
          })
        })
      }
    }
  })
  
  # --- Display question and answer ---
  output$qaDisplay <- renderUI({
    req(current_question())
    qinfo <- current_question()
    cat <- qinfo$category
    idx <- paste0("q", qinfo$index)
    points <- qinfo$index * 100
    qid <- paste0(cat, "_", qinfo$index)
    
    question_text <- questions[[cat]][[idx]]
    answer_text <- answers[[cat]][[idx]]
    
    tagList(
      h3(paste(cat, "-", points, "punti")),
      h4(question_text, style = "color:navy;"),
      if (!show_answer()) {
        actionButton("reveal", "Mostra la risposta", class = "btn-primary")
      } else if (!answered[[qid]]) {
        tagList(
          h4(paste("Risposta:", answer_text), style = "color:darkgreen;"),
          br(),
          fluidRow(
            column(6, actionButton("redPoints", paste("Assegna a 🔴", red_name), class = "btn-danger btn-lg", width = "100%")),
            column(6, actionButton("bluePoints", paste("Assegna a 🔵", blue_name), class = "btn-info btn-lg", width = "100%"))
          )
        )
      } else {
        h4("✅ Domanda già risposta!", style = "color:gray;")
      }
    )
  })
  
  # --- Reveal answer ---
  observeEvent(input$reveal, {
    show_answer(TRUE)
  })
  
  # --- Award points ---
  observeEvent(input$redPoints, {
    qinfo <- current_question()
    req(qinfo)
    qid <- paste0(qinfo$category, "_", qinfo$index)
    if (!answered[[qid]]) {
      scores$red <- scores$red + qinfo$index * 100
      answered[[qid]] <- TRUE
      show_answer(TRUE)
      if (checkGameOver()) game_over(TRUE)
    }
  })
  
  observeEvent(input$bluePoints, {
    qinfo <- current_question()
    req(qinfo)
    qid <- paste0(qinfo$category, "_", qinfo$index)
    if (!answered[[qid]]) {
      scores$blue <- scores$blue + qinfo$index * 100
      answered[[qid]] <- TRUE
      show_answer(TRUE)
      if (checkGameOver()) game_over(TRUE)
    }
  })
  
  # --- Display scores ---
  output$scoreRed <- renderText({ scores$red })
  output$scoreBlue <- renderText({ scores$blue })
  
  # --- Restart game ---
  observeEvent(input$restart, {
    scores$red <- 0
    scores$blue <- 0
    for (cat in categories) {
      for (i in 1:5) {
        answered[[paste0(cat, "_", i)]] <- FALSE
      }
    }
    current_question(NULL)
    show_answer(FALSE)
    game_over(FALSE)
  })
}

# --- Run App ---
shinyApp(ui, server)

