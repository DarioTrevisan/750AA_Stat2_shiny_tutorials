# 750AA Stat2 Shiny Tutorials

A collection of R Shiny apps used for the 750AA Statistica II course: small
step-by-step tutorials plus interactive teaching games covering estimators,
clustering, PCA, KNN, and Bayesian classification.

## Contents

- **`01-app` … `16-app`** — a progression of small, self-contained Shiny
  tutorial apps, each demonstrating one concept (reactivity, plots, tables,
  dashboards, etc.) in a single `app.R`.
- **`click-screen-app`**, **`timer-app`** — small standalone demo apps.
- **`demo-01-WERMS`**, **`demo-02-medoids`**, **`demo-03-clustering`**,
  **`demo-04-PCA`**, **`demo-05-KNN`**, **`demo-06-bayes`** — larger
  teaching apps, each with a Description tab, an interactive Demo tab, and a
  "Play" game tab. Structure: `global.R` loads packages and sources
  `extras.R` (helpers), `ui.R`, and `server.R`; `server.R` sources
  `server_demo.R` and `server_game.R`.
- **`Jeopardy-1`**, **`Jeopardy-2`**, **`jeopardy-3`** — a Jeopardy-style
  quiz game (shared game engine, different quiz content per lecture).

## Running an app

Open any `app.R` (or the folder containing `global.R`/`ui.R`/`server.R`) in
RStudio and click "Run App", or from R:

```r
shiny::runApp("01-app")
```
