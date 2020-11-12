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
library(rintrojs)
library(visNetwork)
library(markdown)
library(V8)
library(lobstr)
library(shinydisconnect)
library(rjson)
library(rgeos)
library(polyclip)
library(colorspace)

# library(dashboardthemes)
options(repos = BiocManager::repositories())
futile.logger::flog.threshold(futile.logger::ERROR, name = "VennDiagramLogger") # to suppress creation of log file

# if (!requireNamespace("BiocManager", quietly = TRUE))
#     install.packages("BiocManager")
# BiocManager::install("fgsea")
# setRepositories(addURLs = c(BioC = "https://bioconductor.org/packages/3.8/bioc"))

#users = reactiveValues(count = 0,ip = c())
ips = c()

source("global/functions.R")
source("global/init.R")


# customTheme <- shinyDashboardThemeDIY(
#   ### general
#   appFontFamily = "Arial"
#   ,appFontColor = "#000000"
#   ,primaryFontColor = "#0F0F0F"
#   ,infoFontColor = "#0F0F0F"
#   ,successFontColor = "#0F0F0F"
#   ,warningFontColor = "#0F0F0F"
#   ,dangerFontColor = "#0F0F0F"
#   ,bodyBackColor = "#FFFFFF"
#
#   ### header
#   ,logoBackColor = "#1976D2"
#
#   ,headerButtonBackColor = "#1976D2"
#   ,headerButtonIconColor = "#DEEDFF"
#   ,headerButtonBackColorHover = "#FFFFFF"
#   ,headerButtonIconColorHover = "#1976D2"
#
#   ,headerBackColor = "#1976D2"
#   ,headerBoxShadowColor = ""
#   ,headerBoxShadowSize = "0px 0px 0px"
#
#   ### sidebar
#   ,sidebarBackColor = "#FFFFFF"
#   ,sidebarPadding = "0"
#
#   ,sidebarMenuBackColor = "transparent"
#   ,sidebarMenuPadding = "0"
#   ,sidebarMenuBorderRadius = 0
#
#   ,sidebarShadowRadius = ""
#   ,sidebarShadowColor = "0px 0px 0px"
#
#   ,sidebarUserTextColor = "#737373"
#
#   ,sidebarSearchBackColor = "#F0F0F0"
#   ,sidebarSearchIconColor = "#646464"
#   ,sidebarSearchBorderColor = "#DCDCDC"
#
#   ,sidebarTabTextColor = "#1976D2"
#   ,sidebarTabTextSize = "14"
#   ,sidebarTabBorderStyle = "none"
#   ,sidebarTabBorderColor = "none"
#   ,sidebarTabBorderWidth = "0"
#
#   ,sidebarTabBackColorSelected = "#DEEDFF"
#   ,sidebarTabTextColorSelected = "#084F96"
#   ,sidebarTabRadiusSelected = "0px"
#
#   ,sidebarTabBackColorHover = "#DEEDFF"
#   ,sidebarTabTextColorHover = "#084F96"
#   ,sidebarTabBorderStyleHover = "none solid none none"
#   ,sidebarTabBorderColorHover = "#1976D2"
#   ,sidebarTabBorderWidthHover = "4"
#   ,sidebarTabRadiusHover = "0px"
#
#   ### boxes
#   ,boxBackColor = "#FFFFFF"
#   ,boxBorderRadius = "5"
#   ,boxShadowSize = "none"
#   ,boxShadowColor = ""
#   ,boxTitleSize = "18"
#   ,boxDefaultColor = "#CFCFCF"
#   ,boxPrimaryColor = "#50A5FA"
#   ,boxInfoColor = "#65D5EB"
#   ,boxSuccessColor = "#70AD47"
#   ,boxWarningColor = "#ED7D31"
#   ,boxDangerColor = "#E84C22"
#
#   ,tabBoxTabColor = "#FFFFFF"
#   ,tabBoxTabTextSize = "14"
#   ,tabBoxTabTextColor = "#4B99E8"
#   ,tabBoxTabTextColorSelected = "#1976D2"
#   ,tabBoxBackColor = "#FFFFFF"
#   ,tabBoxHighlightColor = "#A2C5EB"
#   ,tabBoxBorderRadius = "5"
#
#   ### inputs
#   ,buttonBackColor = "#FFFFFF"
#   ,buttonTextColor = "#1976D2"
#   ,buttonBorderColor = "#1976D2"
#   ,buttonBorderRadius = "5"
#
#   ,buttonBackColorHover = "#E8F1FA"
#   ,buttonTextColorHover = "#1976D2"
#   ,buttonBorderColorHover = "#1976D2"
#
#   ,textboxBackColor = "#FFFFFF"
#   ,textboxBorderColor = "#999999"
#   ,textboxBorderRadius = "5"
#   ,textboxBackColorSelect = "#E8F1FA"
#   ,textboxBorderColorSelect = "#1976D2"
#
#   ### tables
#   ,tableBackColor = "#E8F1FA"
#   ,tableBorderColor = "#FFFFFF"
#   ,tableBorderTopSize = "1"
#   ,tableBorderRowSize = "1"
# )


# js expansion for box collapse
jscode <- "
shinyjs.collapse = function(boxid) {
$('#' + boxid).closest('.box').find('[data-widget=collapse]').click();
}
"


# ===================================================== global.R END ============

# Run the application
#shinyApp(ui = ui, server = server)
