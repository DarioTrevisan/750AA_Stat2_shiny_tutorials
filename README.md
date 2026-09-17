# 750AA Stat2 Shiny Tutorials

A collection of R Shiny interactive teaching games used for the 750AA
Statistica II course, covering estimators, clustering, PCA, KNN, and
Bayesian classification.

## Contents

- **`demo-01-WERMS`**, **`demo-02-medoids`**, **`demo-03-clustering`**,
  **`demo-04-PCA`**, **`demo-05-KNN`**, **`demo-06-bayes`** — larger
  teaching apps, each with a Description tab, an interactive Demo tab, and a
  "Play" game tab. Structure: `global.R` loads packages and sources
  `extras.R` (helpers), `ui.R`, and `server.R`; `server.R` sources
  `server_demo.R` and `server_game.R`.
- **`Jeopardy-1`**, **`Jeopardy-2`**, **`jeopardy-3`** — a Jeopardy-style
  quiz game (shared game engine, different quiz content per lecture).

## Running an app

Open the folder containing `global.R`/`ui.R`/`server.R` (or the `.R` file
for the Jeopardy apps) in RStudio and click "Run App", or from R:

```r
shiny::runApp("demo-01-WERMS")
```
