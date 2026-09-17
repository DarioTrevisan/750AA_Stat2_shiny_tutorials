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

