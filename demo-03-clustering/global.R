library(shiny)
library(ggplot2)
library(ggdendro) # to plot dendrograms with ggplot2
library(cluster)
library(tools)

source("extras.R")
source("ui.R")
source("server.R")

shinyApp(ui, server)
