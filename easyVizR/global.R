# ==== global.R START ===========================================================

# --------------- Load and configure libraries -------------------

library(shiny)
library(ggplot2)
library(DT)
# library(pheatmap)
library(UpSetR)
library(eulerr)
library(VennDiagram)
library(wordcloud)
library(plyr)
library(dplyr)
library(shinydashboard)
library(plotly)
library(shinyWidgets)
library(shinydashboard)
library(shinycssloaders)
library(rvest)
library(htmlwidgets)
library(data.table)
# library(dqshiny)
library(waiter)
library(shinythemes)
library(shinyBS)
library(BiocManager)
library(shinyjs)
options(repos = BiocManager::repositories())
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # to suppress creation of log file

# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("fgsea")
# setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))



source("global/functions.R")
source("global/init.R")




# ===================================================== global.R END ============

# Run the application 
#shinyApp(ui = ui, server = server)
