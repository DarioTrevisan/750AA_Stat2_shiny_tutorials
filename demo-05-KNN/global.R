library(shiny)
library(ggplot2)
library(tools)
library(datasets)
library(tidyr)
library(class)
library(scales)
library(MedDataSets)

source("extras.R")
source("ui.R")
source("server.R")



shinyApp(ui, server)
