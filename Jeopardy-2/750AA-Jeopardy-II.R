library(shiny)

# team names

red_name <- "Gestionali"
blue_name <- "Resto del Mondo"

# main title

titolo <- paste("750AA Statistica II: Jeopardy! Parte 2: Regressione - ", red_name, "vs", blue_name)

# --- Placeholder Questions and Answers ---
categories <- c("Performance", "Generalizzazione", "RegressioneLineare", "Intervalli", "Collinearità")

questions <- list(
  Performance = list(
    q1 = "Che cos’è l’errore quadratico medio (MSE)?",
    q2 = "Come si interpreta l’RMSE rispetto all’MSE?",
    q3 = "Che cosa rappresenta il coefficiente di determinazione R²?",
    q4 = "Qual è la differenza tra RSE e RMSE?",
    q5 = "Quando l’R² può essere fuorviante?"
  ),
  Generalizzazione = list(
    q1 = "Cos’è l’errore di generalizzazione?",
    q2 = "Cosa rappresenta il bias nel compromesso bias-varianza?",
    q3 = "Cosa rappresenta la varianza nel compromesso bias-varianza?",
    q4 = "Come si può stimare l’errore di generalizzazione di un modello di regressione?",
    q5 = "Come si può ridurre l’overfitting nel KNN per la regressione?"
  ),
  RegressioneLineare = list(
    q1 = "Cosa significa OLS (Ordinary Least Squares)?",
    q2 = "Come si ottiene la stima OLS della pendenza nella regressione lineare semplice?",
    q3 = "Cos’è la regressione polinomiale?",
    q4 = "Che cos’è la matrice di Gram e perché è importante?",
    q5 = "Sotto quali ipotesi la stima OLS è pure una stima MLE?"
    ),
  Intervalli = list(
    q1 = "Cosa rappresenta un intervallo di fiducia in regressione?",
    q2 = "Qual è la differenza tra intervallo di fiducia e di previsione?",
    q3 = "Cosa si intende per intervallo di credibilità in un approccio bayesiano?",
    q4 = "Come funziona il bootstrap nella stima degli intervalli?",
    q5 = "Qual è un vantaggio del bootstrap rispetto ai metodi parametrici?"
  ),
  Collinearità = list(
    q1 = "Cos’è la multi-collinearità tra variabili esplicative (fattori di ingresso)?",
    q2 = "Come si misura la multicollinearità?",
    q3 = "Che effetto ha la multicollinearità sui coefficienti stimati?",
    q4 = "Quali tecniche si possono usare per ridurre la collinearità?",
    q5 = "In che modo Ridge e LASSO aiutano a gestire la collinearità?"
    )
)

answers <- list(
  Performance = list(
    q1 = "È la media dei quadrati delle differenze tra i valori osservati e quelli predetti: misura la precisione complessiva del modello.",
    q2 = "L’RMSE è la radice quadrata dell’MSE: ha le stesse unità della variabile di risposta, facilitando l’interpretazione.",
    q3 = "Misura la proporzione di varianza spiegata dal modello rispetto alla varianza totale: varia tra 0 e 1.",
    q4 = "L’RSE (Residual Standard Error) è una stima della deviazione standard degli errori residui e tiene conto del numero di parametri, mentre l’RMSE misura piuttosto la precisione media delle predizioni.",
    q5 = "Quando si confrontano modelli con numero diverso di predittori: R² cresce sempre aggiungendo variabili, anche irrilevanti."
  ),
  Generalizzazione = list(
    q1 = "È la differenza tra la prestazione del modello sui dati di training e su nuovi dati: misura la capacità di generalizzare.",
    q2 = "È l’errore dovuto all’eccessiva semplificazione del modello: un bias alto significa sottostima della complessità.",
    q3 = "È la sensibilità del modello alle variazioni nei dati di training: un’alta varianza implica overfitting.",
    q4   = "Usando la validazione incrociata (cross-validation), per valutare le prestazioni su più suddivisioni del dataset.",
    q5   = "Aumentando K, normalizzando le variabili e utilizzando pesi che diminuiscono con la distanza."
  ),
  RegressioneLineare = list(
    q1 = "È il metodo dei minimi quadrati ordinari, che minimizza la somma dei quadrati degli errori per stimare i coefficienti del modello lineare.",
    q2 = "È data da β_1 = cor(x,y) sd(y)/sd(x).",
    q3 = "È una regressione lineare sui termini polinomiali di una variabile: ad esempio, includendo x², x³ per modellare relazioni non lineari.",
    q4 = "È la matrice XᵀX: deve essere invertibile per stimare i coefficienti. Se quasi singolare, indica collinearità tra variabili.",
    q5 = "Serve che i residui siano gaussiani centrati a varianza costante (omoschedasticità) e non correlati. Queste ipotesi garantiscono che OLS sia MLE."  
    ),
  Intervalli = list(
    q1 = "È l’intervallo in cui ci si aspetta che cada il vero parametro con una certa frequenza (es. 95%).",
    q2 = "L’intervallo di fiducia riguarda i parametri del modello, quello di previsione riguarda nuove osservazioni e include anche la variabilità residua.",
    q3 = "È l’intervallo che contiene una certa probabilità a posteriori del parametro, data l’evidenza osservata.",
    q4 = "Si ottengono molte stime del parametro tramite campionamento con ripetizione, costruendo un intervallo empirico dai quantili.",
    q5 = "Non richiede assunzioni forti sulla distribuzione degli errori o dei parametri."
  ),
  Collinearità = list(
    q1 = "È la presenza di forte correlazione lineare tra due o più variabili predittive, che rende instabili le stime OLS.",
    q2 = "Attraverso il VIF (Variance Inflation Factor): valori superiori a 5 o 10 indicano problemi di multicollinearità.",
    q3 = "Aumenta la varianza delle stime, rendendole meno affidabili e più sensibili ai dati.",
    q4 = "Rimozione di variabili ridondanti, combinazione tramite PCA o regolarizzazione con Ridge/LASSO.",
    q5 = "Aggiungono penalizzazioni sui coefficienti: Ridge li riduce (L2), LASSO può azzerarli (L1), migliorando la stabilità del modello."
  )
)

# --- UI ---
ui <- fluidPage(
  titlePanel(paste("🎯", titolo)),

  uiOutput("mainUI")
)

# --- SERVER ---
server <- function(input, output, session) {

  n_questions <- 5

  # qid identifies a single question, e.g. "Bayes_3"
  make_qid <- function(category, index) paste0(category, "_", index)
  points_for <- function(index) index * 100

  current_question <- reactiveVal(NULL)
  show_answer <- reactiveVal(FALSE)
  game_over <- reactiveVal(FALSE)

  scores <- reactiveValues(red = 0, blue = 0)
  answered <- reactiveValues()

  # Check if game is over (all questions answered)
  checkGameOver <- function() {
    all(vapply(names(answered), function(qid) answered[[qid]], logical(1)))
  }

  # --- Per-question setup ---
  # For every question we: mark it unanswered, render its board tile as its
  # own output, and wire up its click/reveal/scoring observers. Each question
  # gets its own uiOutput/observers (rather than sharing IDs across questions
  # or regenerating the whole board on every answer) so that recreating one
  # button never resets another button's click counter — with shared IDs,
  # Shiny ignores a click that resends the same value it last sent, which
  # made buttons silently stop responding after the board redrew.
  # local() gives each iteration its own copies of qid/points/etc., since a
  # for-loop variable in R is shared across iterations and would otherwise be
  # captured by reference (every observer would see the value from the last
  # iteration) once these reactive blocks actually run.
  for (category in categories) {
    for (i in seq_len(n_questions)) {
      local({
        qid <- make_qid(category, i)
        points <- points_for(i)
        this_category <- category
        this_index <- i

        answered[[qid]] <- FALSE

        # Board tile: point value, or "—" once answered
        output[[paste0("tile_", qid)]] <- renderUI({
          actionButton(
            inputId = qid,
            label = if (answered[[qid]]) "—" else as.character(points),
            width = "100%",
            style = paste0(
              "margin-bottom:10px; height:60px; font-size:20px;",
              if (answered[[qid]]) "background-color:lightgray;"
            )
          )
        })

        # Clicking a tile opens its question
        observeEvent(input[[qid]], {
          if (!answered[[qid]] && !game_over()) {
            current_question(list(category = this_category, index = this_index))
            show_answer(FALSE)
          }
        })

        # Reveal button for this question
        observeEvent(input[[paste0("reveal_", qid)]], {
          show_answer(TRUE)
        })

        # Award points to a team for this question (once only)
        award_points <- function(team) {
          if (!answered[[qid]]) {
            scores[[team]] <- scores[[team]] + points
            answered[[qid]] <- TRUE
            if (checkGameOver()) game_over(TRUE)
          }
        }
        observeEvent(input[[paste0("red_", qid)]], award_points("red"))
        observeEvent(input[[paste0("blue_", qid)]], award_points("blue"))
      })
    }
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
          lapply(categories, function(category) {
            column(2,
                   tags$h4(category, align = "center"),
                   lapply(seq_len(n_questions), function(i) {
                     uiOutput(paste0("tile_", make_qid(category, i)))
                   })
            )
          })
        )
      )
    }
  })

  # --- Display question and answer ---
  output$qaDisplay <- renderUI({
    req(current_question())
    qinfo <- current_question()
    category <- qinfo$category
    index <- qinfo$index
    qid <- make_qid(category, index)
    points <- points_for(index)

    question_text <- questions[[category]][[paste0("q", index)]]
    answer_text <- answers[[category]][[paste0("q", index)]]

    tagList(
      h3(paste(category, "-", points, "punti")),
      h4(question_text, style = "color:navy;"),
      if (!show_answer()) {
        actionButton(paste0("reveal_", qid), "Mostra la risposta", class = "btn-primary")
      } else if (!answered[[qid]]) {
        tagList(
          h4(paste("Risposta:", answer_text), style = "color:darkgreen;"),
          br(),
          fluidRow(
            column(6, actionButton(paste0("red_", qid), paste("Assegna a 🔴", red_name), class = "btn-danger btn-lg", width = "100%")),
            column(6, actionButton(paste0("blue_", qid), paste("Assegna a 🔵", blue_name), class = "btn-info btn-lg", width = "100%"))
          )
        )
      } else {
        h4("✅ Domanda già risposta!", style = "color:gray;")
      }
    )
  })

  # --- Display scores ---
  output$scoreRed <- renderText({ scores$red })
  output$scoreBlue <- renderText({ scores$blue })

  # --- Restart game ---
  observeEvent(input$restart, {
    scores$red <- 0
    scores$blue <- 0
    for (category in categories) {
      for (i in seq_len(n_questions)) {
        answered[[make_qid(category, i)]] <- FALSE
      }
    }
    current_question(NULL)
    show_answer(FALSE)
    game_over(FALSE)
  })
}

# --- Run App ---
shinyApp(ui, server)
