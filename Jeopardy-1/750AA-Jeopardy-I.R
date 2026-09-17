library(shiny)

# team names

red_name <- "Gestionali"
blue_name <- "Resto del Mondo"

# main title

titolo <- paste("750AA Statistica II: Jeopardy!", red_name, "vs", blue_name)

# --- Placeholder Questions and Answers ---
categories <- c("Stimatori", "Clustering", "PCA", "Classificazione", "Bayes")

questions <- list(
  Stimatori = list(
    q1 = "Qual è la definizione di media aritmetica e come si calcola?",
    q2 = "In che modo la mediana differisce dalla media? In quale situazione la mediana è preferita?",
    q3 = "Spiega come si calcolano i quantili e fornisci un esempio pratico.",
    q4 = "Qual è la relazione tra la media e la varianza in un dataset? Come influenzano i valori estremi?",
    q5 = "Proponi un esempio in cui la mediana fornisca un'informazione migliore rispetto alla media e giustifica la tua scelta."
  ),
  Clustering = list(
    q1 = "Cos'è il clustering e qual è il suo obiettivo principale?",
    q2 = "Descrivi l'algoritmo K-means e spiega come determina il numero di cluster.",
    q3 = "Qual è la differenza tra K-means e PAM (Partitioning Around Medoids)?",
    q4 = "Cosa sono AGNES e DIANA? In che modo differiscono nel loro approccio al clustering gerarchico?",
    q5 = "In quali situazioni preferiresti utilizzare PAM rispetto a K-means e perché?"
  ),
  PCA = list(
    q1 = "Cos'è l'analisi delle componenti principali (PCA) e qual è il suo scopo principale?",
    q2 = "Spiega la differenza tra PCA e analisi fattoriale esplorativa (EFA).",
    q3 = "Quali sono i passi principali per eseguire una PCA? Descrivi brevemente ciascun passo.",
    q4 = "Come si interpreta il lo scree plot? Cosa significa un alto valore di varianza spiegata?",
    q5 = "Fornisci un esempio di quando sarebbe appropriato utilizzare l'EFA piuttosto che la PCA."
  ),
  Classificazione = list(
    q1 = "Cos'è il K-Nearest Neighbors (KNN) e come funziona?",
    q2 = "Quali sono alcuni indicatori di performance per valutare un modello di classificazione? Spiega brevemente ciascuno.",
    q3 = "Cosa rappresenta la curva ROC e come viene utilizzata per confrontare modelli di classificazione?",
    q4 = "Descrivi come si costruisce e si interpreta una matrice di confusione.",
    q5 = "In che modo la scelta del valore di K in KNN influisce sulla performance del modello?"
  ),
  Bayes = list(
    q1 = "Cos'è il teorema di Bayes e come viene applicato nel contesto della classificazione?",
    q2 = "Descrivi l'algoritmo Naive Bayes e il significato dell'ipotesi di indipendenza.",
    q3 = "Qual è la differenza tra LDA e QDA? In quali situazioni uno è preferito rispetto all'altro?",
    q4 = "Spiega come funziona la regressione logistica e come viene utilizzata per la classificazione.",
    q5 = "Fornisci un esempio pratico in cui un modello Naive Bayes potrebbe fallire e spiega perché."
  )
)

answers <- list(
  Stimatori = list(
    q1 = "Media aritmetica = somma dei valori divisa per il numero totale di osservazioni.",
    q2 = "La mediana è il valore centrale dei dati ordinati; è preferita in presenza di outlier.",
    q3 = "I quantili dividono i dati (ordinati) in proporzione (es. il 25° percentile è il valore sotto cui cade il 25% dei dati).",
    q4 = "La varianza misura la dispersione intorno alla media; outlier aumentano sia media sia varianza.",
    q5 = "Nel reddito di una popolazione, la mediana è più rappresentativa della media, che è influenzata da redditi estremi."
  ),
  Clustering = list(
    q1 = "Il clustering raggruppa dati simili per scoprire strutture o pattern nascosti.",
    q2 = "K-means minimizza la distanza media dai centroidi; il numero di cluster è fissato a priori (K).",
    q3 = "PAM usa medoid invece di centroidi, più robusti agli outlier rispetto a K-means.",
    q4 = "AGNES è un metodo agglomerativo (unisce cluster), DIANA è divisivo (divide cluster grandi).",
    q5 = "PAM è preferito con dati rumorosi o outlier, perché usa medoid reali e non medie."
  ),
  PCA = list(
    q1 = "La PCA riduce la dimensionalità proiettando i dati su componenti che spiegano la massima varianza.",
    q2 = "La PCA descrive varianza totale, l’EFA cerca fattori latenti che spiegano covarianze tra variabili.",
    q3 = "Passi: standardizzazione → calcolo covarianza → autovalori/autovettori → scelta componenti → interpretazione.",
    q4 = "Il grafico mostra la varianza spiegata da ciascuna componente; valori alti indicano informazione concentrata lungo la componente.",
    q5 = "L’EFA è adatta quando si ipotizzano variabili latenti sottostanti (es. tratti psicologici)."
  ),
  Classificazione = list(
    q1 = "KNN classifica un punto in base alla maggioranza dei suoi K vicini più prossimi.",
    q2 = "Indicatori: accuratezza, precisione, recall, F1-score, AUC — misurano correttezza e bilanciamento.",
    q3 = "La curva ROC mostra sensibilità vs 1-specificità; l’AUC confronta modelli: più alto è meglio.",
    q4 = "La matrice di confusione confronta classi vere e predette per analizzare errori di classificazione.",
    q5 = "K piccolo = modello sensibile ma rumoroso; K grande = più stabile ma meno preciso."
  ),
  Bayes = list(
    q1 = "Il teorema di Bayes aggiorna la probabilità di una classe in base all’evidenza osservata.",
    q2 = "Naive Bayes assume indipendenza tra le feature; calcola P(classe|dati) con Bayes.",
    q3 = "LDA assume covarianze uguali tra classi, QDA no; QDA gestisce meglio confini non lineari.",
    q4 = "La regressione logistica stima la probabilità di una classe tramite uno score lineare nelle log-odds.",
    q5 = "Naive Bayes fallisce se le feature sono fortemente correlate, violando l’indipendenza."
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
