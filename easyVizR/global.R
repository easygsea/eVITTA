# ==== global.R START ===========================================================

# --------------- Load and configure libraries -------------------
if(!requireNamespace("BiocManager", quietly = TRUE))
  install.packages("BiocManager")
if (!require('shiny')){install.packages('shiny')}
if (!require('ggplot2')){install.packages('ggplot2')}
if (!require('DT')){install.packages('DT')}
if (!require('UpSetR')){install.packages('UpSetR')}
if (!require('eulerr')){install.packages('eulerr')}
if (!require('VennDiagram')){install.packages('VennDiagram')}
if (!require('wordcloud')){install.packages('wordcloud')}
if (!require('plyr')){install.packages('plyr')}
if (!require('dplyr')){install.packages('dplyr')}
if (!require('shinydashboard')){install.packages('shinydashboard')}
if (!require('plotly')){install.packages('plotly')}
if (!require('shinyWidgets')){install.packages('shinyWidgets')}
if (!require('shinycssloaders')){install.packages('shinycssloaders')}
if (!require('rvest')){install.packages('rvest')}
if (!require('htmlwidgets')){install.packages('htmlwidgets')}
if (!require('data.table')){install.packages('data.table')}
if (!require('waiter')){install.packages('waiter')}
if (!require('shinythemes')){install.packages('shinythemes')}
if (!require('shinyBS')){install.packages('shinyBS')}
if (!require('BiocManager')){install.packages('BiocManager')}
if (!require('shinyjs')){install.packages('shinyjs')}
if (!require('rintrojs')){install.packages('rintrojs')}
if (!require('visNetwork')){install.packages('visNetwork')}
if (!require('markdown')){install.packages('markdown')}
if (!require('V8')){install.packages('V8')}
if (!require('lobstr')){install.packages('lobstr')}
if (!require('shinydisconnect')){install.packages('shinydisconnect')}
if (!require('rjson')){install.packages('rjson')}
if (!require('rgeos')){install.packages('rgeos')}
if (!require('polyclip')){install.packages('polyclip')}
if (!require('colorspace')){install.packages('colorspace')}
if (!require('RColorBrewer')){install.packages('RColorBrewer')}
if (!require('RRHO')){install.packages('RRHO')}

library(shiny)
library(ggplot2)
library(DT)
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
library(RColorBrewer)
library(RRHO)

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
