output$correlogram <- renderUI({
  
  rownames(rv$corrDatasetRepresentation) <- rv$corrDatasetRepresentation$datasetName
  
  fluidRow(
    column(4,
       box(
         title = NULL, status = "primary", solidHeader = FALSE, width = 12,
         fluidRow(
           column(12,
                  checkboxGroupInput(
                    "corrVarSelected",
                    "Show correlogram for:",
                    choices = NULL,
                    selected = rv$corrVarSelected,
                    inline = FALSE,
                    width = NULL,
                    choiceNames = rv$corrDatasetRepresentation$displayName,
                    choiceValues = rv$corrDatasetRepresentation$datasetName
                  )
           )
         ),
         fluidRow(
           column(12,
                  radioGroupButtons("corrDataOptions",
                                    label = HTML(paste0(
                                      "<b>Data Options:</b>",
                                      add_help("corrDataOptions_help", style="margin-left: 5px;"))
                                    ),
                                    choices = c("All data", "Intersection only"),
                                    selected=rv$corrDataOptions,size="s", direction="horizontal"), 
                  bsTooltip("nxy_sc_dflogic_help",
                            dflogic_explanation,
                            placement = "right"),
                  radioTooltip(id = "corrDataOptions", choice = "All data", 
                               title = dflogic_explanation_5, 
                               placement = "right"),
                  radioTooltip(id = "corrDataOptions", choice = "Intersection only", 
                               title = dflogic_explanation_1, 
                               placement = "right"),
           )
         ),
         fluidRow(
           column(12,
                  div(
                    style="display: inline-block;",
                    div(style="display: inline-block;vertical-align:middle; width: 10em;",HTML(paste("<b>Use Abbreviation:</b> ", add_help("corrUseAbbreviation_help", style="margin-left: 5px;")))),
                    div(style="display: inline-block; width: 5em; margin-left: 0.5em",
                        materialSwitch(
                          "corrUseAbbreviation",
                          # label = HTML(paste("<b>Use Abbreviation:</b> ", add_help("corrUseAbbreviation_help", style="margin-left: 5px;"))),
                          label = NULL,
                          value = rv$corrUseAbbreviation,
                          status = "default",
                          right = TRUE,
                          inline = FALSE,
                          width = NULL
                        )
                    )
                  )
           )
         ),
         fluidRow(
           column(12,
                  div(
                    style="display: inline-block;",
                    div(style="display: inline-block;vertical-align:middle; width: 10em;",HTML(paste("<b>Interactive Plot:</b> ", add_help("corrInteractivePLot_help", style="margin-left: 5px;")))),
                    div(style="display: inline-block; width: 5em; margin-left: 0.5em",
                        materialSwitch(
                          "corrInteractivePlot",
                          label = NULL,
                          value = rv$corrInteractivePlot,
                          status = "default",
                          right = TRUE,
                          inline = FALSE,
                          width = NULL
                        )
                    )
                  )
           )
         ),
         fluidRow(
           column(12,
                  radioButtons(
                    "corrPlotType",
                    label= HTML(paste0(
                      "<b>Plot Type:</b>",
                      add_help("corrPlotType_help", style="margin-left: 5px;"))
                    ),
                    choices = c("Heatmap", "Correlogram"),
                    selected = rv$corrPlotType,
                    inline = FALSE,
                    width = NULL,
                    choiceNames = NULL,
                    choiceValues = NULL
                  )
           )
         ),
         
         uiOutput("heatmapCorrelogramOptions"),
         uiOutput("replotButton"),
           
       ),
       
       bsTooltip("corrDataOptions_help", 
                 "&#34;Intersection only&#34; draws a correlogram/heatmap of the datasets excluding the filtered-out rows",  
                 placement = "top"),
       bsTooltip("corrUseAbbreviation_help", 
                 "Abbreviates dataset names to DataSetA, DataSetB, DataSetC, etc. Useful if dataset names are too long.",  
                 placement = "top"),
       bsTooltip("corrInteractivePLot_help", 
                 "If toggled on, the plot will be interactive. You can hover over the plot to see specific values.",  
                 placement = "top"),
       bsTooltip("corrPlotType_help", 
                 "Choose between heatmap and correlogram", 
                 placement = "top")
    ),
    column(8,
       # Main correlogram
       uiOutput("correlogramDisplay"),
       
       # Exclusion report
       box(
         title = span( icon("exclamation"), "Exclusion Report"), status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
         wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 30em;",
                   uiOutput("corrDroppedRows"))
       )
    )
  )
})

output$correlogramDisplay <- renderUI({
  if (rv$corrInteractivePlot == TRUE) {
    if (rv$corrUseAbbreviation == TRUE) {
      box(
        title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
        plotlyOutput("correlogramPlotly", height = "40em"),
        div(style="text-align: center; margin-top: 1.5em", uiOutput("Legend")),
        downloadButton('corrDownloadPlotly', 'Download Plot')
      )
    } else {
      box(
        title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
        plotlyOutput("correlogramPlotly", height = "40em"),
        downloadButton('corrDownloadPlotly', 'Download Plot')
      )
    }
    
  } else {
    
    if (rv$corrUseAbbreviation == TRUE) {
      box(
        title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
        plotOutput("correlogramPlot", height = "40em"),
        div(style="text-align: center; margin-top: 1.5em", uiOutput("Legend")),
        downloadButton('corrDownloadPlot', 'Download Plot')
      )
    } else {
      box(
        title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
        plotOutput("correlogramPlot", height = "40em"),
        downloadButton('corrDownloadPlot', 'Download Plot')
      )
    }
    
  }
})

output$heatmapCorrelogramOptions <- renderUI({
  
  # All this because Bivariate density plot doesn't work in plotly. I want to keep it for static plots.
  if (input$corrInteractivePlot == TRUE) {
    corrUpperLowerOptions = c('points', 'smooth', 'cor', 'blank')
    corrUpperLowerNamedOptions = c("Scatter plot" = 'points',
                                   "Scatter plot with a smoothed line" = 'smooth',
                                   "Correlation value plot" = 'cor',
                                   "Blank plot" = 'blank')
    
  } else {
    corrUpperLowerOptions = c('points', 'smooth', 'density', 'cor', 'blank')
    corrUpperLowerNamedOptions = c("Scatter plot" = 'points',
                                   "Scatter plot with a smoothed line" = 'smooth',
                                   "Bivariate density plot" = 'density',
                                   "Correlation value plot" = 'cor',
                                   "Blank plot" = 'blank')
  }
  
  if (input$corrPlotType == 'Heatmap') {
    div(
      fluidRow(
        column(12,
               div(style="display: inline-block; vertical-align:middle; width: 8em",HTML(paste("<b>Correlate By:</b>", add_help("corrCorrelateBy_help", style="margin-left: 5px;")))),
               div(style="display: inline-block;",
                   pickerInput(
                     "corrCorrelateBy",
                     NULL,
                     choices = c('R Value' = 'pearson', 'RHO Value' = 'spearman', 'P Value Pearson' = 'pValPearson', 'P Value Spearman' = 'pValSpearman'),
                     selected = rv$corrCorrelateBy,
                     multiple = FALSE,
                   )
               )
        )
      ),
      fluidRow(
        column(12,
           div(
             style="display: inline-block;",
             materialSwitch(
               "corrShowCorrelationValue",
               label = HTML(paste("<b>Show Correlation value:</b> ", add_help("corrShowCorrelationValue_help", style="margin-left: 5px;"))),
               value = rv$corrShowCorrelationValue,
               status = "default",
               right = FALSE,
               inline = FALSE,
               width = NULL
             )
           ),
           
           bsTooltip("corrShowCorrelationValue_help", 
                     "Display the correlation value on each heatmap tile", 
                     placement = "top"),
           bsTooltip("corrCorrelateBy_help", 
                     "Correlate with rValue (Pearson) or rhoValue (Spearman)", 
                     placement = "top")
        )
      )
    )
  } else if (input$corrPlotType == 'Correlogram') {
    div(
      fluidRow(
        column(12,
               div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Upper Triangle:</strong>"))),
               div(style="display: inline-block;vertical-align:middle",HTML(paste(add_help("corrUpper_help", style="margin-left: 5px;")))),
               div(style="display: inline-block; width: 13em;",
                   pickerInput(
                     "corrUpper",
                     NULL,
                     choices = corrUpperLowerNamedOptions,
                     selected = rv$corrUpperV,
                     multiple = FALSE,
                     choicesOpt = list(
                       disabled = corrUpperLowerOptions %in% c(rv$corrLowerV)
                     )
                   )
               )
        )
      ),
      fluidRow(
        column(12,
               div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Diagonal:</strong>"))),
               div(style="display: inline-block;vertical-align:middle",HTML(paste(add_help("corrDiag_help", style="margin-left: 5px;")))),
               div(style="display: inline-block; width: 13em;",
                   pickerInput(
                     "corrDiag",
                     NULL,
                     choices = c("Univariate density plot" = 'densityDiag',
                                 "Bar plot" = 'barDiag',
                                 "Blank plot" = 'blankDiag'),
                     selected = rv$corrDiagV,
                     multiple = FALSE
                   )
               )
        )
      ),
      fluidRow(
        column(12,
               div(style="display: inline-block;vertical-align:middle; width: 7em;",HTML(paste("<strong>Lower Triangle:</strong>"))),
               div(style="display: inline-block;vertical-align:middle",HTML(paste(add_help("corrLower_help", style="margin-left: 5px;")))),
               div(style="display: inline-block; width: 13em;",
                   pickerInput(
                     "corrLower",
                     NULL,
                     choices = corrUpperLowerNamedOptions,
                     selected = rv$corrLowerV,
                     multiple = FALSE,
                     choicesOpt = list(
                       disabled = corrUpperLowerOptions %in% c(rv$corrUpperV)
                     )
                   )
               )
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
    
  }
})

output$replotButton <- renderUI({
  if (input$corrInteractivePlot == TRUE) {
    hardLimit = 1000000; # deactivate replotButton
    softLimit = 500000; # warn the user of the runtime of the correlation
  } else {
    hardLimit = 3000000; # deactivate replotButton
    softLimit = 1500000; # warn the user of the runtime of the correlation
    # hardLimit = 35000; # deactivate replotButton
    # softLimit = 20000; # warn the user of the runtime of the correlation
  }
  
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
        fluidRow(column(12,disabled(actionButton(inputId = "corrReplot", label = "Replot!")))),
        fluidRow(
          column(12,
             div(
               style="display: inline-block; margin-top: 1.5rem",
               box(
                 title = NULL, background = "red", solidHeader = TRUE, width=12,
                 HTML("<text style='color:white'>Correlation too large! Either deselect datasets or change the settings of 'upper' and 'lower'.<br>
                     You can also turn interactive plot off. <br>
                     Settings in order from simplest to most complex: blank, cor, points, smooth, density.")
               )
             )
          )
        )
      )
    } else if (nrow(colsWanted[names(selected)]) * length(selected) * runTimeFactorSum > softLimit){
      # Soft limit
      div(
        fluidRow(column(12, actionButton(inputId = "corrReplot", label = "Replot!"))),
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
  
  if (rv$corrUseAbbreviation == TRUE) {
    namedListOfDatasets <- corrDatasetRepresentation$abbreviation
  } else {
    namedListOfDatasets <- corrDatasetRepresentation$datasetName
  }

  
  names(namedListOfDatasets) <- corrDatasetRepresentation$datasetName
  names(selected) <- namedListOfDatasets[selected]
  to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic,
                                 gls = n_ins_gls(),
                                 user_criteria = rv$ins_criteria,
                                 starting_df = df_n_basic())
  
  if (dataOptions == "All data") {
    colsWanted <- df_n[grepl("\\<Stat", names(df_n))]
    colsWanted <- colsWanted[complete.cases(colsWanted),]
  } else if (dataOptions == "Intersection only") {
    colsWanted <- to_plot_df[grepl("\\<Stat", names(to_plot_df))]
  }
  
  if (rv$corrUseAbbreviation == TRUE) {
    colnames(colsWanted) <- corrDatasetRepresentation$abbreviation
  } else {
    colnames(colsWanted) <- corrDatasetRepresentation$datasetName
  }

  
  if (plotType == "Heatmap") {
    if (correlateBy == "pearson" || correlateBy == "spearman") {
      corrMatrix <- round(cor(colsWanted[names(selected)], method = correlateBy), 3)[ ,length(selected):1]
    } else if (correlateBy == "pValPearson") {
      corrMatrix <- rcorr(as.matrix(colsWanted[names(selected)]),type="pearson")$P[ ,length(selected):1]
    } else if (correlateBy == "pValSpearman") {
      corrMatrix <- rcorr(as.matrix(colsWanted[names(selected)]),type="spearman")$P[ ,length(selected):1]
    }

    if (rv$corrUseAbbreviation == TRUE) {
      corrLabelsSize = 12
      corrLabelsAngle = 45
    } else {
      corrLabelsSize = 8
      corrLabelsAngle = 25
    }
    
    if (rv$corrInteractivePlot == TRUE) {
        ggplotly(
          ggcorrplot(corrMatrix, hc.order = FALSE, type = "full", colors = c("blue", "WhiteSmoke", "red"), outline.col = "white", lab = showCorrelationValue, tl.cex = corrLabelsSize, digits = 10, tl.srt = corrLabelsAngle)
        )
    } else {
        ggcorrplot(corrMatrix, hc.order = FALSE, type = "full", colors = c("blue", "WhiteSmoke", "red"), outline.col = "white", lab = showCorrelationValue, tl.cex = corrLabelsSize, digits = 10, tl.srt = corrLabelsAngle)
    }
  } else if (plotType == "Correlogram") {
    if (rv$corrInteractivePlot == TRUE) {
      ggplotly(
        ggpairs(colsWanted[names(selected)], title=NULL,
                upper = list(continuous = upper),
                lower = list(continuous = lower),
                diag = list(continuous = diag))
      )
    } else {
      ggpairs(colsWanted[names(selected)], title=NULL,
              upper = list(continuous = upper),
              lower = list(continuous = lower),
              diag = list(continuous = diag))
    }
  }
}


# Static correlogram   
output$correlogramPlot <- renderPlot({
  selected <- rv$corrVarSelected
  draw_correlogram(selected, rv$corrDatasetRepresentation, rv$df_n)
})

# Interactive correlogram
output$correlogramPlotly <- renderPlotly({
  selected <- rv$corrVarSelected
  draw_correlogram(selected, rv$corrDatasetRepresentation, rv$df_n)
})

correlogramPlotlyReactive <- reactive({
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
  paste(row["Name"], " <text style='color:DarkGray'>(Missing values in ", paste(naValues, collapse = ", "),")</text>")
}

# Dropped Rows
output$corrDroppedRows <- renderUI({
  df_n <- rv$df_n
  selected <- rv$corrVarSelected
  colsWanted <- df_n[grepl("^Stat|Name", names(df_n))]
  selectedColumns <- colsWanted[c("Name",paste("Stat_",selected, sep=""))]
  dropped_rows <- selectedColumns[!complete.cases(selectedColumns),]
  
  if (rv$corrDataOptions == "All data") {
    HTML(paste0("<strong>", nrow(dropped_rows), " rows were ommited because they have missing values</strong><br>", 
                paste(apply(dropped_rows, 1, printExcludedRow), collapse = "<br>")))
  } else if (rv$corrDataOptions == "Intersection only") {
    to_plot_df <- get_df_by_dflogic(selected, dflogic = rv$nxy_sc_dflogic,
                                     gls = n_ins_gls(),
                                     user_criteria = rv$ins_criteria,
                                     starting_df = df_n_basic())
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
  filename = function() {
    paste("correlogram-", Sys.Date(), ".png", sep = "")
  },
  content = function(file) {
    ggsave(file, device = png, dpi = 600)
  }
)

output$corrDownloadPlotly <- downloadHandler(
  filename = function() {
      paste("correlogram-", Sys.Date(), ".html", sep = "")
    },  
  content = function(file) {
    saveWidget(as_widget(correlogramPlotlyReactive()), file, selfcontained = TRUE)
    }
  )