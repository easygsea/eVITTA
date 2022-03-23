output$correlogram <- renderUI({
  
  rownames(rv$corrDatasetRepresentation) <- rv$corrDatasetRepresentation$datasetName
  
  fluidRow(
    column(4,
           box(
             title = NULL, status = "primary", solidHeader = FALSE, width = 12,
             checkboxGroupInput(
               "corrVarSelected",
               "Show correlogram for:",
               choices = NULL,
               selected = rv$corrVarSelected,
               inline = FALSE,
               width = NULL,
               choiceNames = rv$corrDatasetRepresentation$displayName,
               choiceValues = rv$corrDatasetRepresentation$datasetName
             ),
             radioButtons(
               "corrDataOptions",
               # "Data Options:",
               label = HTML(paste0(
                 "<b>Data Options:</b>",
                 add_help("corrDataOptions_help", style="margin-left: 5px;"))
               ),
               choices = c("All data", "Intersection only"),
               selected = rv$corrDataOptions,
               inline = FALSE,
               width = NULL,
               choiceNames = NULL,
               choiceValues = NULL
             ),
             radioButtons(
               "corrPlotType",
               # "Plot Type:",
               label= HTML(paste0(
                 "<b>Plot Type:</b>",
                 add_help("corrPlotType_help", style="margin-left: 5px;"))
               ),
               choices = c("Heatmap", "correlogram"),
               selected = rv$corrPlotType,
               inline = FALSE,
               width = NULL,
               choiceNames = NULL,
               choiceValues = NULL
             ),
             
             
             conditionalPanel(
               condition = "input.corrPlotType == 'Heatmap'",
               
               fluidRow(
                 column(12,
                        div(style="display: inline-block; vertical-align:middle; width: 8em",HTML(paste("<b>Correlate By:</b>", add_help("corrCorrelateBy_help", style="margin-left: 5px;")))),
                        div(style="display: inline-block;",
                            pickerInput(
                              "corrCorrelateBy",
                              NULL,
                              choices = c('rValue', 'rhoValue'),
                              selected = rv$corrCorrelateBy,
                              multiple = FALSE,
                            )
                        ),
                        # div(style="display: inline-block;", add_help("corrCorrelateBy_help", style="margin-left: 5px;")),
                 )
               ),
               fluidRow(
                 column(12,
                    div(
                       style="display: inline-block;",
                       materialSwitch(
                         "corrShowCorrelationValue",
                         label = HTML(paste("<b>Show Correlation value:</b> ", add_help("corrShowCorrelationValue_help", style="margin-left: 5px;"))),
                         # label= HTML(paste0(
                         #   "<b>Show Stats:</b>",
                         #   add_help("corrShowCorrelationValue_help", style="margin-left: 5px;"))
                         # ),
                         value = rv$corrShowCorrelationValue,
                         status = "default",
                         right = FALSE,
                         inline = FALSE,
                         width = NULL
                       )
                    ),
                   # add_help("corrDataOptions_help", style="margin-left: 5px;"),
                   # div(style="display: inline-block;", add_help("corrShowCorrelationValue_help", style="margin-left: 5px;")),
                 )
               )
               
             ),
             uiOutput("selectPlotMode"),
             
             uiOutput("replotButton"),
             
             # You can move these elsewhere in this scope
             bsTooltip("corrDataOptions_help", 
                       "&#34;Intersection only&#34; draws a correlogram/heatmap of the datasets excluding the filtered-out rows",  
                       placement = "top"),
             bsTooltip("corrPlotType_help", 
                       "Choose between heatmap and correlogram", 
                       placement = "top"),
             bsTooltip("corrShowCorrelationValue_help", 
                       "Display the correlation value on each heatmap tile", 
                       placement = "top"),
             bsTooltip("corrCorrelateBy_help", 
                       "Correlate with rValue (Pearson) or rhoValue (Spearman)", 
                       placement = "top"),
             
           )
    ),
    column(8,
           # Main correlogram
           # box(
           #   title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
           #   plotOutput("correlogramPlot", height = "40em"),
           #   div(style="text-align: center", uiOutput("Legend")),
           #   downloadButton('corrDownloadPlot', 'Download Plot')
           # ),
           uiOutput("correlogramDisplay"),
           
           
           # Exclusion report
           box(
             title = span( icon("exclamation"), "Exclusion Report"), status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
             wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 30em;",
                       uiOutput("corrDroppedRows"))
           )
           # shinydashboard::box() could be useful if box() didn't work
    )
  )
  
})

output$correlogramDisplay <- renderUI({
  box(
    title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
    plotOutput("correlogramPlot", height = "40em"),
    div(style="text-align: center", uiOutput("Legend")),
    downloadButton('corrDownloadPlot', 'Download Plot')
  )
})

output$selectPlotMode <- renderUI({
  req(input$corrPlotType == 'correlogram')
  div(
    fluidRow(
      column(12,
             div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Upper:</strong>", add_help("corrUpper_help", style="margin-left: 5px;")))),
             div(style="display: inline-block; width: 8em;",
                 pickerInput(
                   "corrUpper",
                   NULL,
                   choices = c('points', 'smooth', 'density', 'cor', 'blank'),
                   selected = rv$corrUpperV,
                   # selected = 'cor',
                   multiple = FALSE,
                   choicesOpt = list(
                     disabled = c('points', 'smooth', 'density', 'cor', 'blank') %in% c(rv$corrLowerV)
                     # disabled = c('points', 'smooth', 'smooth_loess', 'density', 'cor', 'blank') %in% c(input$lower)
                   )
                 )
             )
             # div(style="display: inline-block;", add_help("corrUpper_help", style="margin-left: 5px;"))
      )
    ),
    fluidRow(
      column(12,
             div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Diagonal:</strong>", add_help("corrDiag_help", style="margin-left: 5px;")))),
             div(style="display: inline-block; width: 8em;",
                 pickerInput(
                   "corrDiag",
                   NULL,
                   choices = c('densityDiag', 'barDiag', 'blankDiag'),
                   selected = rv$corrDiagV,
                   multiple = FALSE
                 )
             )
             # div(style="display: inline-block;", add_help("corrDiag_help", style="margin-left: 5px;"))
      )
    ),
    fluidRow(
      column(12,
             div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Lower:</strong>", add_help("corrLower_help", style="margin-left: 5px;")))),
             div(style="display: inline-block; width: 8em;",
                 pickerInput(
                   "corrLower",
                   NULL,
                   choices = c('points', 'smooth', 'density', 'cor', 'blank'),
                   selected = rv$corrLowerV,
                   # selected = 'points',
                   multiple = FALSE,
                   choicesOpt = list(
                     disabled = c('points', 'smooth', 'density', 'cor', 'blank') %in% c(rv$corrUpperV)
                     # disabled = c('points', 'smooth', 'density', 'cor', 'blank') %in% c(input$upper)
                   )
                 )
             )
             # div(style="display: inline-block;", add_help("corrLower_help", style="margin-left: 5px;"))
      )
    ),
    bsTooltip("corrUpper_help", 
              "Select the plot type of the upper-right section of the correlogram", 
              placement = "top"),
    bsTooltip("corrDiag_help", 
              "Select the plot type of the diagonal line in the centre of the correlogram",
              placement = "top"),
    bsTooltip("corrLower_help", 
              "Select the plot type of the lower-left section of the correlogram", 
              placement = "top"),
  )
})

output$replotButton <- renderUI({
  # hardLimit = 3000000; # deactivate replotButton
  # softLimit = 1500000; # warn the user of the runtime of the correlation
  hardLimit = 35000; # deactivate replotButton
  softLimit = 20000; # warn the user of the runtime of the correlation
  
  correlogramModesRuntimeFactor <- list(blank = 0,
                                        cor = 2, 
                                        points = 3,
                                        smooth = 3,
                                        density = 5)
  
  rownames(rv$corrDatasetRepresentation) <- rv$corrDatasetRepresentation$datasetName
  corrDatasetRepresentation <- rv$corrDatasetRepresentation
  namedListOfDatasets <- corrDatasetRepresentation$abbreviation
  names(namedListOfDatasets) <- corrDatasetRepresentation$datasetName
  selected <- input$corrVarSelected
  names(selected) <- namedListOfDatasets[selected]
  
  if (is.null(input$corrUpper) | is.null(input$corrLower)) {
    runTimeFactorSum = 0
  } else {
    runTimeFactorSum = as.numeric(correlogramModesRuntimeFactor[input$corrUpper]) + as.numeric(correlogramModesRuntimeFactor[input$corrLower])
  }
  
  df_n <- rv$df_n
  
  
  to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic,
                                  gls = n_ins_gls(),
                                  user_criteria = rv$ins_criteria,
                                  starting_df = df_n_basic())
                                  
  if (input$corrDataOptions == "All data") {
    colsWanted <- df_n[grepl("\\<Stat", names(df_n))]
  } else if (input$corrDataOptions == "Intersection only") {
    colsWanted <- to_plot_df[grepl("\\<Stat", names(to_plot_df))]
  }
  
  colnames(colsWanted) <- corrDatasetRepresentation$abbreviation

  if (input$corrPlotType == "Heatmap") {
    actionButton(inputId = "corrReplot", label = "Replot!")
  } else {
    if (nrow(colsWanted[names(selected)]) * length(selected) * runTimeFactorSum > hardLimit) {
      # Hard limit
      div(
        fluidRow(
          column(12,
             disabled(
               actionButton(inputId = "corrReplot", label = "Replot!")
             )
          )
        ),
        fluidRow(
          column(12,
             div(
               style="display: inline-block; margin-top: 1.5rem",
               box(
                 title = NULL, background = "red", solidHeader = TRUE, width=12,
                 HTML("<text style='color:white'>Correlation too large! Either deselect datasets or change the settings of 'upper' and 'lower'.<br>
                     Settings in order from simplest to most complex: blank, cor, points, smooth, density.")
               )
             )
          )
        )
      )
    } else if (nrow(colsWanted[names(selected)]) * length(selected) * runTimeFactorSum > softLimit){
      # Soft limit
      div(
        fluidRow(
          column(12,
               actionButton(inputId = "corrReplot", label = "Replot!")
          )
        ),
        fluidRow(
          column(12,
             div(
               style="display: inline-block; margin-top: 1.5rem",
               box(
                 title = NULL, background = "orange", solidHeader = TRUE, width=12,
                 HTML("<text style='color:white'>Warning: Correlation may take too long")
               )
             )
          )
        )
      )
    } else {
      actionButton(inputId = "corrReplot", label = "Replot!")
    }
  }
  
  
  
  
  
})



draw_correlogram <- function(selected,
                             corrDatasetRepresentation,
                             df_n = rv$df_n,
                             varSelected = rv$corrVarSelected,
                             dataOptions = rv$corrDataOptions,
                             plotType = rv$corrPlotType,
                             correlateBy = rv$corrCorrelateBy,
                             showCorrelationValue = rv$corrShowCorrelationValue,
                             upper = rv$corrUpper,
                             lower = rv$corrLower,
                             diag = rv$corrDiag) {
  
  
  # Function starts here
  namedListOfDatasets <- corrDatasetRepresentation$abbreviation
  names(namedListOfDatasets) <- corrDatasetRepresentation$datasetName
  names(selected) <- namedListOfDatasets[selected]
  
  to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic,
                                 gls = n_ins_gls(),
                                 user_criteria = rv$ins_criteria,
                                 starting_df = df_n_basic()
  )
  
  
  if (dataOptions == "All data") {
    # colsWanted <- df_n[grepl("\\<Value", names(df_n))]
    colsWanted <- df_n[grepl("\\<Stat", names(df_n))]
  } else if (dataOptions == "Intersection only") {
    # colsWanted <- to_plot_df[grepl("\\<Value", names(to_plot_df))]
    colsWanted <- to_plot_df[grepl("\\<Stat", names(to_plot_df))]
  }
  
  colnames(colsWanted) <- corrDatasetRepresentation$abbreviation
  
  if (plotType == "Heatmap") {
    if (correlateBy == "rValue") {
      # Defaults to pearson correlation
      ggcorr(colsWanted[names(selected)], label = showCorrelationValue, label_round = 3)
    } else if (correlateBy == "rhoValue") {
      ggcorr(colsWanted[names(selected)], label = showCorrelationValue, method = c("pairwise.complete.obs", "spearman"), label_round = 3)
    } else if (correlateBy == "logPValue") {
      # To Do
      # ggcorr(colsWanted[names(selected)], label = showCorrelationValue, method = c("pairwise.complete.obs", "spearman"), label_round = 3)
    }
  } else if (plotType == "correlogram") {
    ggpairs(colsWanted[names(selected)], title=NULL,
            upper = list(continuous = upper),
            lower = list(continuous = lower),
            diag = list(continuous = diag))
    # + ggplot2::theme(aspect.ratio = 1)
  }
}

# Correlogram    
output$correlogramPlot <- renderPlot({
  
  selected <- rv$corrVarSelected
  
  draw_correlogram(selected, rv$corrDatasetRepresentation, rv$df_n)
  
})

# Legend
output$Legend <- renderUI({
  namedListOfDisplayName <- rv$corrDatasetRepresentation$displayName
  names(namedListOfDisplayName) <- rv$corrDatasetRepresentation$datasetName
  
  HTML(paste0("<strong>Legend</strong>: <br>", paste(namedListOfDisplayName[rv$corrVarSelected], collapse = "<br>")))
})

printExcludedRow <- function(row) {
  naValues <- names(row)[is.na(row)]
  #<font color=\"#FF0000\">
  paste(row["Name"], " <text style='color:DarkGray'>(Missing values in ", paste(naValues, collapse = ", "),")</text>")
}

# Dropped Rows
output$corrDroppedRows <- renderUI({
  
  df_n <- rv$df_n
  
  selected <- rv$corrVarSelected
  colsWanted <- df_n[grepl("^Stat|Name", names(df_n))]
  selectedColumns <- colsWanted[c("Name",paste("Stat_",selected, sep=""))]
  # selectedColumns <- colsWanted[c("Name",paste("Stat_",selected, sep=""))]
  
  dropped_rows <- selectedColumns[!complete.cases(selectedColumns),]
  
  
  if (rv$corrDataOptions == "All data") {
    # dropped_rows <- selectedColumns[!complete.cases(selectedColumns),]
    
    HTML(paste0("<strong>", nrow(dropped_rows), " rows were ommited because they have missing values</strong><br>", 
                paste(apply(dropped_rows, 1, printExcludedRow), collapse = "<br>")))
    
  } else if (rv$corrDataOptions == "Intersection only") {
    to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic,
                                     gls = n_ins_gls(),
                                     user_criteria = rv$ins_criteria,
                                     starting_df = df_n_basic()
    )
    
    # dropped_rows <- union(selectedColumns[!complete.cases(selectedColumns),], setdiff(df_n[,1], to_plot_df[,1]))
    filtered_out_rows <- setdiff(df_n[, 1], to_plot_df[, 1])
    
    HTML(paste0("<strong>"), (nrow(dropped_rows) + length(filtered_out_rows)), " rows were ommited in total<br>",
         nrow(dropped_rows)," rows were ommmited because they have missing values<br>",
         length(filtered_out_rows)," rows were ommmited because they are filtered out<br>",
         "Rows with missing values:</strong><br>",
         paste(apply(dropped_rows, 1, printExcludedRow), collapse = "<br>"),
         "<br><strong>Filtered out rows:</strong><br>",
         paste(filtered_out_rows, collapse = "<br>"))
  }
  
})

output$corrDownloadPlot <- downloadHandler(
  filename = "plot.png" ,
  content = function(file) {
    #ggsave(p(), filename = file)
    selected <- rv$corrVarSelected
    
    png(file)
    print(draw_correlogram(selected, rv$corrDatasetRepresentation, df_n))
    dev.off()
  }
)