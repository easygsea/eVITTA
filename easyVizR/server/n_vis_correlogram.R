# --------- correlogram limits --------- #
# A limit is placed on correlogram but not the heatmap, 
# this is deduced by plot runtime factor and data size

# runtime factor
# See: runTimeFactorSum = as.numeric(correlogramModesRuntimeFactor[input$corrUpper]) + as.numeric(correlogramModesRuntimeFactor[input$corrLower])

correlogramModesRuntimeFactor <- list(blank = 0,
                                      cor = 0.2, 
                                      points = 3,
                                      smooth = 3,
                                      density = 5)

# data size 
# see: nrow(colsWanted[names(selected)]) * length(selected) * runTimeFactorSum > hardLimit
corrHardLimitInteractive <- 1000000; # deactivate replotButton
corrSoftLimitInteractive <- 200000; # warn the user of the runtime of the correlation
corrHardLimitStatic <- corrHardLimitInteractive*5; # deactivate replotButton
corrSoftLimitStatic <- corrSoftLimitInteractive*5; # warn the user of the runtime of the correlation


reportVariables <- function(varList){
  printList <- unlist(lapply(varList, function(x){
    varName = deparse(substitute(x))
    varVal = toString(x)
    paste0(varName, ": ", varVal)
  }))
  print("Reporting variables:")
  for(x in printList){ print(x) }
}



output$correlogram <- renderUI({
  
  rownames(rv$corrDatasetRepresentation) <- rv$corrDatasetRepresentation$datasetName
  
  fluidRow(
    column(4,
       box(
         title = NULL, status = "primary", solidHeader = FALSE, width = 12,
         fluidRow(
           column(12,
                  div(id="n_cor1",checkboxGroupInput(
                    "corrVarSelected",
                    "Show correlogram for:",
                    choices = NULL,
                    selected = rv$corrVarSelected,
                    inline = FALSE,
                    width = NULL,
                    choiceNames = rv$corrDatasetRepresentation$displayName,
                    choiceValues = rv$corrDatasetRepresentation$datasetName
                  ))
           )
         ),
         fluidRow(
           column(12,
                  div(id="n_cor2",radioGroupButtons("corrDataOptions",
                                    label = HTML(paste0(
                                      "<b>Data Options:</b>",
                                      add_help("corrDataOptions_help", style="margin-left: 5px;"))
                                    ),
                                    choices = c("All data", "Intersection only"),
                                    selected=rv$corrDataOptions,size="s", direction="horizontal")), 
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
                  div(id="n_cor3",radioButtons(
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
                  ))
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
       box(
         title = span( icon("chart-area"), "Correlogram"), status = "primary", solidHeader = FALSE, width = 12,
         uiOutput("correlogramDisplay"),
         uiOutput("correlogramLegends"),
         uiOutput("correlogramDownloadButton")
       ),
       
       # Exclusion report
       box(
         title = span( icon("exclamation"), "Exclusion Report"), status = "warning", solidHeader = FALSE, width = 12, collapsible = TRUE, collapsed = TRUE,
         wellPanel(id = "tPanel", style = "overflow-y:scroll; max-height: 30em;",
                   uiOutput("corrDroppedRows"))
       )
    )
  )
})
output$correlogramLegends <- renderUI({
  req(rv$corrUseAbbreviation == TRUE)
  div(style="text-align: center; margin-top: 1.5em", uiOutput("Legend"))
})
output$correlogramDownloadButton <- renderUI({
  if (rv$corrInteractivePlot == TRUE) {
    downloadButton('corrDownloadPlotly', 'Download Plot')
  } else {
    downloadButton('corrDownloadPlot', 'Download Plot')
  }
})
output$correlogramDisplay <- renderUI({
  if (rv$corrInteractivePlot == TRUE) {
      plotlyOutput("correlogramPlotly", height = "40em")
  } else {
      plotOutput("correlogramPlot", height = "40em")
  }
})

output$heatmapCorrelogramOptions <- renderUI({
  if (input$corrPlotType == 'Heatmap') {
    uiOutput("heatmapOptions")
  } else if (input$corrPlotType == 'Correlogram') {
    uiOutput("correlogramOptions")
  }
})

output$heatmapOptions <- renderUI({
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
})

output$correlogramOptions <- renderUI({
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
})



output$replotButton <- renderUI({
  req(length(input$corrVarSelected)>=2)
  
  if (input$corrInteractivePlot == TRUE) {
    hardLimit = corrHardLimitInteractive; # deactivate replotButton
    softLimit = corrSoftLimitInteractive; # warn the user of the runtime of the correlation
  } else {
    hardLimit = corrHardLimitStatic; # deactivate replotButton
    softLimit = corrSoftLimitStatic; # warn the user of the runtime of the correlation
  }
  
  rownames(rv$corrDatasetRepresentation) <- rv$corrDatasetRepresentation$datasetName
  corrDatasetRepresentation <- rv$corrDatasetRepresentation
  namedListOfDatasets <- corrDatasetRepresentation$abbreviation
  names(namedListOfDatasets) <- corrDatasetRepresentation$datasetName
  selected <- input$corrVarSelected
  names(selected) <- namedListOfDatasets[selected]
  

  
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
  
  # compute runtime factor
  if (is.null(input$corrUpper) | is.null(input$corrLower)) {
    runTimeFactorSum = 0
  } else {
    # data factor = data points in each pairwise plot
    dataFactor <- nrow(colsWanted[names(selected)]) * 2
    # number of plots factor = number of plots on each diagonal
    plotsFactor <- sum(1:length(selected)-1)
    # plot type factor
    upperPlotTypeFactor <- as.numeric(correlogramModesRuntimeFactor[input$corrUpper])
    lowerPlotTypeFactor <- as.numeric(correlogramModesRuntimeFactor[input$corrLower])
    # compute total factor based on 3 values above
    upperRuntimeFactor <- dataFactor * plotsFactor * upperPlotTypeFactor
    lowerRuntimeFactor <- dataFactor * plotsFactor * lowerPlotTypeFactor
    
    runTimeFactorSum = upperRuntimeFactor + lowerRuntimeFactor
    # reportVariables(list(dataFactor, plotsFactor, upperPlotTypeFactor, lowerPlotTypeFactor, 
    #                 upperRuntimeFactor, lowerRuntimeFactor, runTimeFactorSum))
  }
  
  

  if (input$corrPlotType == "Heatmap") {
    actionButton(inputId = "corrReplot", label = "Replot!")
  } else {
    if (runTimeFactorSum > hardLimit) {
      # Hard limit
      div(
        fluidRow(column(12,
                        disabled(actionButton(inputId = "corrReplot", label = "Replot!"))
                        )),
        fluidRow(
          column(12,
             div(
               style="display: inline-block; margin-top: 1.5rem",
               box(
                 title = NULL, background = "red", solidHeader = TRUE, width=12,
                 HTML("<text style='color:white'>Correlogram is disabled for overly large datasets. <br>
                 The following are recommended: <br>
                 1. Deselect some datasets; <br>
                 2. Change the settings of 'upper' and 'lower'; <br>
                 3. Turn off interactive plot;<br>
                 4. Directly download plots without rendering.")
               )
             ),
             div(
               HTML("<b>Download Plot without Rendering:<b/>"),br(),
               downloadButton('corrDownloadPlotPreview', 'Static'),
               downloadButton('corrDownloadPlotlyPreview', 'Interactive')
             )
          )
        )
      )
    } else if (runTimeFactorSum > softLimit){
      # Soft limit
      div(
        fluidRow(column(12, 
                        actionButton(inputId = "corrReplot", label = "Replot!"),
                        )),
        fluidRow(
          column(12,
             div(
               style="display: inline-block; margin-top: 1.5rem",
               box(
                 title = NULL, background = "orange", solidHeader = TRUE, width=12,
                 HTML("<text style='color:white'>Warning: Plot will take a while to render. Please be patient.")
               )
             ),
             div(
               HTML("<b>Download Plot without Rendering:<b/>"),br(),
               downloadButton('corrDownloadPlotPreview', 'Static'),
               downloadButton('corrDownloadPlotlyPreview', 'Interactive')
             )
          )
        )
      )
    } else {
      div(
        actionButton(inputId = "corrReplot", label = "Replot!")
      )
      
    }
  }
  
})


# ------------------------------------------------------------------------
#                             UI ENDS
# ------------------------------------------------------------------------


draw_correlogram <- function(selected,
                             corrDatasetRepresentation,
                             df_n = rv$df_n,
                             varSelected = rv$corrVarSelected,
                             dataOptions = rv$corrDataOptions,
                             plotType = rv$corrPlotType,
                             correlateBy = rv$corrCorrelateBy,
                             showCorrelationValue = rv$corrShowCorrelationValue,
                             corrUseAbbreviation = rv$corrUseAbbreviation,
                             upper = rv$corrUpper,
                             lower = rv$corrLower,
                             diag = rv$corrDiag) {
  
  if (corrUseAbbreviation == TRUE) {
    namedListOfDatasets <- corrDatasetRepresentation$abbreviation
  } else {
    namedListOfDatasets <- corrDatasetRepresentation$datasetName
  }
  
  withProgress(message = "Drawing Plot ...",value = 0,{
    
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
      colsWanted <- colsWanted[complete.cases(colsWanted),]
    }
    
    if (corrUseAbbreviation == TRUE) {
      colnames(colsWanted) <- corrDatasetRepresentation$abbreviation
    } else {
      colnames(colsWanted) <- corrDatasetRepresentation$datasetName
    }
    
    incProgress(0.2)
    
    if (plotType == "Heatmap") {
      if (correlateBy == "pearson" || correlateBy == "spearman") {
        corrMatrix <- round(cor(colsWanted[names(selected)], method = correlateBy), 3)[ ,length(selected):1]
      } else {
        if (correlateBy == "pValPearson") {
          corrMatrix <- rcorr(as.matrix(colsWanted[names(selected)]),type="pearson")$P[ ,length(selected):1]
        } else if (correlateBy == "pValSpearman") {
          corrMatrix <- rcorr(as.matrix(colsWanted[names(selected)]),type="spearman")$P[ ,length(selected):1]
        }
        corrMatrix[corrMatrix <= 1E-10] <- 1E-10 # set min p-value for log transform
        corrMatrix <- -log10(corrMatrix)
        # corrMatrix[] <- vapply(corrMatrix, negativeLog, numeric(1))
        # apply -log(pValue) to the whole matrix
      }
      
      if (corrUseAbbreviation == TRUE) {
        corrLabelsSize = 12
        corrLabelsAngle = 45
      } else {
        corrLabelsSize = 8
        corrLabelsAngle = 25
      }
      
      if (correlateBy == "pearson"){ corrColorbarTitle = "R" } 
      else if (correlateBy == "spearman"){ corrColorbarTitle = "Rho" } 
      else if (correlateBy == "pValPearson"){ corrColorbarTitle = "-log10(PValue)" } 
      else if (correlateBy == "pValSpearman"){ corrColorbarTitle = "-log10(PValue)" } 
      
      if (correlateBy == "pValPearson" || correlateBy == "pValSpearman") {
        out <- ggcorrplot(
          corrMatrix,
          hc.order = FALSE,
          type = "full",
          outline.col = "white",
          lab = showCorrelationValue,
          tl.cex = corrLabelsSize,
          digits = 10,
          tl.srt = corrLabelsAngle) + scale_fill_gradient(
            name = corrColorbarTitle,
            low = "WhiteSmoke", 
            high = "red",
            limit=c(0, NA)
          ) 
        
      } else {
        out <- ggcorrplot(
          corrMatrix,
          hc.order = FALSE,
          type = "full",
          outline.col = "white",
          lab = showCorrelationValue,
          tl.cex = corrLabelsSize,
          digits = 10,
          tl.srt = corrLabelsAngle) + scale_fill_gradient2(
            name = corrColorbarTitle,
            low = "blue",
            mid = "WhiteSmoke",
            high = "red",
            limit=c(-1, 1)
          ) 
      }
      
    } else if (plotType == "Correlogram") {
      if(diag=="barDiag"){
        diagSettings <- list(continuous = wrap(diag, binwidth=0.1))
      } else {
        diagSettings <- list(continuous = wrap(diag))
      }
      
      out <- suppressWarnings(ggpairs(colsWanted[names(selected)], title=NULL,
                     upper = list(continuous = upper),
                     lower = list(continuous = lower),
                     diag = diagSettings
                     ))
    }
    
    incProgress(0.6)
    out
    
    
  })
}

# negativeLog <- function(value) {
#   value <- -log10(value)
# }


# Static correlogram   
output$correlogramPlot <- renderPlot({
  selected <- rv$corrVarSelected
  suppressMessages(
    draw_correlogram(selected, rv$corrDatasetRepresentation, rv$df_n)
  )
})

# Interactive correlogram
output$correlogramPlotly <- renderPlotly({
  suppressWarnings(correlogramPlotlyReactive())
  
})

correlogramPlotlyReactive <- reactive({
  selected <- rv$corrVarSelected
  suppressWarnings(
    ggplotly(draw_correlogram(selected, rv$corrDatasetRepresentation, rv$df_n))
  )
})

correlogramPlotReactivePreview <- reactive({
  suppressWarnings(draw_correlogram(rv$corrVarSelected, 
                                    rv$corrDatasetRepresentation, 
                                    rv$df_n,
                                    varSelected = input$corrVarSelected,
                                    dataOptions = input$corrDataOptions,
                                    plotType = input$corrPlotType,
                                    correlateBy = input$corrCorrelateBy,
                                    showCorrelationValue = input$corrShowCorrelationValue,
                                    corrUseAbbreviation = input$corrUseAbbreviation,
                                    upper = input$corrUpper,
                                    lower = input$corrLower,
                                    diag = input$corrDiag

  ))
})

correlogramPlotlyReactivePreview <- reactive({
  suppressWarnings(ggplotly(draw_correlogram(rv$corrVarSelected, 
                                             rv$corrDatasetRepresentation, 
                                             rv$df_n,
                                             varSelected = input$corrVarSelected,
                                             dataOptions = input$corrDataOptions,
                                             plotType = input$corrPlotType,
                                             correlateBy = input$corrCorrelateBy,
                                             showCorrelationValue = input$corrShowCorrelationValue,
                                             corrUseAbbreviation = input$corrUseAbbreviation,
                                             upper = input$corrUpper,
                                             lower = input$corrLower,
                                             diag = input$corrDiag
                                               
    ))
    )

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
    ggsave(file, device = png, dpi = 600,
           width= 7, height= 7)
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

output$corrDownloadPlotPreview <- downloadHandler(
  filename = function() { paste("correlogram-", Sys.Date(), ".png", sep = "") },
  content = function(file) {
    req(is.null(n_venn_plt())==F)
    ggsave(file, plot = correlogramPlotReactivePreview(), device = "png", dpi = 600,
           width= 7, height= 7)
  }
)



output$corrDownloadPlotlyPreview <- downloadHandler(
  filename = function() {
    paste("correlogram-", Sys.Date(), ".html", sep = "")
  },  
  content = function(file) {
    saveWidget(as_widget(suppressMessages(correlogramPlotlyReactivePreview())), file, selfcontained = TRUE)
  }
)
