library(fgsea)
library(data.table)
library(msigdbr)
library(dqshiny)

# ==== server.R START ===========================================================
# Define server logic 
# To access any input use input$[inputId] 
#                     ex. input$G_groups (the first select input value)
# To assign any output use output$[outputId] output$
#                      ex. output$myplot (assign the plot output)
server <- function(input, output, session) {
    

    
    
    ####---------------------- REACTIVE VALUES---------------------------####
    # only show button when file is uploaded (Example)
    
    rv <- reactiveValues(upload_state = NULL, ll=ll, gg=gg, tt=tt,
                         upload_columns=NULL, df=NULL,
                         genelist=NA
                         )
    
    
    
    
    
    
    
    
    #======================================================================#
    ####                      ORGANIZE FILES                            ####
    #======================================================================#

    ####---------------------- load file --------------------------####
    
    #---------------------- events ---------------------------#
    
    # when file is uploaded, update state 
    observeEvent(input$file, {
        inFile <- input$file
        rv$upload_columns <- colnames(read.csv(inFile$datapath, nrows=1))
        rv$upload_state <- 'uploaded'
    })
    # when user presses reset, update state
    observeEvent(input$reset, {
        rv$upload_state <- 'reset'
    })
    
    # upon submitting, add file to list of dataframes to select from
    observeEvent(input$submit, {
        inFile <- input$file
        in_df <- read.csv(inFile$datapath)
        # replace the important column names to prevent error later on
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$gene_column, "X")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$Stat_column, "Stat")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$p_column, "PValue")
        colnames(in_df) <- replace(colnames(in_df), colnames(in_df)==input$q_column, "FDR")
        load_cols_list <- c(c("X", "Stat", "PValue", "FDR"),input$load_other_cols)
        #print(load_cols_list)
        in_df <- in_df[,load_cols_list]
        in_df <- remove_nas(in_df)
        
        # set data type
        in_df$X <- as.character(in_df$X) # convert name column to character
        in_df[load_cols_list[-1]] <- sapply(in_df[load_cols_list[-1]],as.numeric) # convert all but name to numeric

        # update rv
        rv$gg <- c(rv$gg, list(in_df))
        rv$ll <- c(rv$ll, input$uploaded_file_name)
        rv$tt <- c(rv$tt, isolate(input$Stat_name))
        
        rv$upload_state <- "reset"
    })
    
    
    
    #----------------------Sidebar UI---------------------------#
    
    # THESE WILL APPEAR AFTER UPLOAD.
    
    # display uploaded file name
    output$uploaded_file <- renderText({
        if (is.null(rv$upload_state)){return (NULL)}
        else if (rv$upload_state == 'reset'){return (NULL)}
        else if (length(rv$upload_columns)<4){returntext <- paste("WARNING: Columns missing! (minimum 4)<br> ", input$file$name, "<br><br>")}
        else{returntext <- paste("Uploaded file: ", input$file$name,"<br><br>")}
        return(returntext)
    })
    
    # input filename
    output$uploaded_file_name <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        textInput(
            inputId = "uploaded_file_name",
            label = "Name:",
            value= input$file$name)
    })
    
    # text instructions 
    output$cn_ins <- renderText({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        "<strong>Select columns corresponding to each element:<strong><br><br>"
    })
    
    
    
    # select columns
    output$cn_1 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        fluidRow(
            box(width = 12, 
                splitLayout(
                    radioButtons(
                        inputId = "gene_column",
                        label = "Gene column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(gene_alias,rv$upload_columns) 
                    ),
                    radioButtons(
                        inputId = "Stat_column",
                        label = "Stat column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(stat_alias,rv$upload_columns)
                    ),
                    textInput(
                        inputId = "Stat_name",
                        label = "Name this stat:",
                        value = itemmatched(stat_alias,rv$upload_columns))
        )))
    })
    output$cn_2 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        fluidRow(
            box(width = 12, 
                splitLayout(
                    radioButtons(
                        inputId = "p_column",
                        label = "P column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(p_alias,rv$upload_columns)
                    ),
                    radioButtons(
                        inputId = "q_column",
                        label = "FDR column:",
                        choices = rv$upload_columns,
                        selected = firstmatch(q_alias,rv$upload_columns)
                    )
                )))
    })
    
    # multiselect to load other columns
    output$load_other_cols <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        multiInput(inputId = "load_other_cols",
                   label = "Load additional columns:",
                   choices = setdiff(rv$upload_columns, c(input$gene_column, input$Stat_column,
                                                          input$p_column,input$q_column)),
                   width = "500px",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Click to select:",
                       selected_header = "Load these additional columns:")
        )
    })
    
    output$cn_feedback <- renderText({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        
        if (length(unique(c(input$gene_column, input$Stat_column, input$p_column, input$q_column)))<4){
            "WARNING: Did you select a column twice?"
        }
    })
    
    # upload / reset buttons
    output$cn_3 <- renderUI({
        req(rv$upload_state == 'uploaded')
        req(length(rv$upload_columns)>=4)
        req(length(unique(c(input$gene_column, input$Stat_column, input$p_column, input$q_column)))==4)
        
        fluidRow(
            box(width = 12, 
                splitLayout(
                    actionButton('submit', 'Confirm and load'),
                    actionButton('reset', 'Reset')
                )))
    })
    
    
    ####---------------------- delete file --------------------------####
    
    #---------------------- events ---------------------------#
    
    observeEvent(input$delete_deg_confirm, {
        #print(input$delete_deg)
        to_delete_i <- which(rv$ll %in% input$delete_deg)
        #print(to_delete_i)
        # delete the items
        rv$ll <- rv$ll[-to_delete_i]
        rv$gg <- rv$gg[-to_delete_i]
    })
    
    #---------------------- Sidebar UI ---------------------------#
    
    output$delete_deg <- renderUI({
        req(length(rv$ll) >= 1)
        multiInput(inputId = "delete_deg",
                   label = "Delete DEG list(s):",
                   choices = rv$ll,
                   width = "500px",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Click to select:",
                       selected_header = "Delete these:")
        )
    })
    output$delete_deg_confirm <- renderUI({
        req(length(input$delete_deg) >= 1)
        req(length(rv$ll) >= 1)
        actionButton("delete_deg_confirm", "Confirm and delete")
    })
    

    
    
    
    #======================================================================#
    ####                         SINGLE WAY                             ####
    #======================================================================#
    
    
    
    ####---------------------- Events ---------------------------####
    
    # update variables
    observe({
        if(is.null(input$df_x_p)==F) {rv$df_x_p <- input$df_x_p}
        if(is.null(input$df_x_q)==F) {rv$df_x_q <- input$df_x_q}
        if(is.null(input$df_x_Stat)==F) {rv$df_x_Stat <- input$df_x_Stat}
        
        if(is.null(input$fs_volcano_p)==F) {rv$fs_volcano_p <- input$fs_volcano_p}
        if(is.null(input$fs_volcano_Stat)==F) {rv$fs_volcano_Stat <- input$fs_volcano_Stat}
        
        if(is.null(input$df_gl_p)==F) {rv$df_gl_p <- input$df_gl_p}
        if(is.null(input$df_gl_q)==F) {rv$df_gl_q <- input$df_gl_q}
        if(is.null(input$df_gl_Stat)==F) {rv$df_gl_Stat <- input$df_gl_Stat}
        
        if(is.null(input$gl_volcano_p)==F) {rv$gl_volcano_p <- input$gl_volcano_p}
        if(is.null(input$gl_volcano_Stat)==F) {rv$gl_volcano_Stat <- input$gl_volcano_Stat}
        
        if(is.null(input$p1_bar_data)==F) {rv$p1_bar_data <- input$p1_bar_data}
        if(is.null(input$p1_bar_sig)==F) {rv$p1_bar_sig <- input$p1_bar_sig}
        
    })
    
    # update current view
    current<- reactive({
        if (is.null(input$show_df)){return(1)}
        else {return({match(input$show_df,rv$ll)})}
    })
    
    
    # submit gene list
    observeEvent(input$submit_genelist,{
        if(input$submit_genelist==0) {rv$genelist <- NULL}
        else if(input$submit_genelist>0) {
            # gets the gene list and splits it to a vector. add [[1]] right after '\\s+' for list
            # needs to be done in one line to work
            print(input$genelist_p1)
            rv$genelist <- isolate(as.list(strsplit(input$genelist_p1, '\\n+'))) 
        }
    })
    
    
    
    # submit full data
    observeEvent(input$submit_x,{
        #print("submit full data: mode")
        
        rv$mode <- isolate(input$mode)
        withProgress(message = 'Updating data...', value = 0, {
            incProgress(0.2)
            
            df <- isolate(rv$gg[[current()]])
            # set data type
            df$X <- as.character(df$X) # convert name column to character
            df[-1] <- sapply(df[-1],as.numeric) # convert all but name to numeric
            
            
            incProgress(0.2)
            
            # initialize graphing params
            rv$show_df <- isolate(input$show_df)
            rv$x_i <- isolate(current())
            
            rv$df_x_p <- 0.05
            rv$df_x_q <- 1
            rv$df_x_Stat <- 0
            
            rv$fs_volcano_p <-0.05
            rv$fs_volcano_Stat <-0
            rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
            rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
            incProgress(0.2)
            
            rv$df <- df
            incProgress(0.2)
        })
    })
    
    # submit gene list
    observeEvent(input$submit_genelist,{
        #print("submit genelist: mode")

        rv$mode <- isolate(input$mode)
        withProgress(message = 'Updating data...', value = 0, {
            incProgress(0.2)
            
            df <- isolate(rv$gg[[current()]])
            incProgress(0.2)
            
            # initialize graphing params
            rv$show_df <- isolate(input$show_df)
            
            rv$df_gl_p <- 1
            rv$df_gl_q <- 1
            rv$df_gl_Stat <- 0
            
            rv$gl_volcano_p <-0.05
            rv$gl_volcano_Stat <-0
            rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
            rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
            
            # bar params
            rv$gl_cols <- isolate(colnames(rv$gg[[current()]])[-1])
            rv$p1_bar_data <- rv$gl_cols[[1]]
            rv$p1_bar_sig <- "PValue"
            
            incProgress(0.2)
            
            # trim by gene list
            print(rv$genelist[[1]])
            df <- df[df$X %in% rv$genelist[[1]],]
            df <- df[order(match(df$X, rv$genelist[[1]])), ]
            incProgress(0.2)
            
            rv$df <- df

        })
    })
    
    
    
    ####---------------------- Sidebar ---------------------------####
    
    # select data to show
    output$select_df <- renderUI({
        selectInput(
            inputId = "show_df",
            label = "Select from uploaded datasets:",
            choices = rv$ll,
            selectize = FALSE) # show duplicate entries
    })
    
    # select subset mode
    output$deg_subset_mode <- renderUI({
        req(length(rv$ll) >= 1)
        req(is.null(input$show_df)==F)
        radioButtons(
            inputId = "mode",
            label = "Subset data:",
            choices = c("All genes", "List of genes"))
    })
    
    # show subset options and submit button
    output$x_confirm <- renderUI({
        req(length(rv$ll) >= 1)
        req(is.null(input$show_df)==F)
        req(is.null(input$mode)==F)
        
        if(input$mode== "All genes"){
            actionButton("submit_x", "Visualize!")
        }
        else if(input$mode== "List of genes"){
            fluidRow(
                box(width=12,
                    textAreaInput("genelist_p1", "Input gene list (separated by new line):", ""),
                    actionButton("submit_genelist", "Visualize!")
                    )
            )
        }
    })

    
    
    ####----------------------Main Tabs---------------------------####
    
    # main tabs
    output$single_main <- renderUI({
        req(is.null(rv$mode)==F)
        if (rv$mode == "All genes"){
            tabsetPanel(
                tabPanel("Table",

                         dataTableOutput("single_tbl") %>% withSpinner(type=4),
                         
                         div(
                             dropdown(
                                 sliderInput("df_x_p", "Select P cutoff:", min = 0.01, max = 1,
                                             value = 0.05, step = 0.01),
                                 sliderInput("df_x_q", "Select FDR cutoff:", min = 0.01, max = 1,
                                             value = 1, step = 0.01),
                                 sliderInput("df_x_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                                             value = 0, step = 0.25)
                                 ,
                                 size = "xs",
                                 icon = icon("cut", class = "opt"),
                                 up = TRUE
                             )
                         ),
                         div(
                             dropdown(
                                 downloadButton("downloaddf","Download table"),
                                 downloadButton("downloadRNK","Download RNK")
                                 ,
                                 size = "xs",
                                 icon = icon("download", class = "opt"),
                                 up = TRUE
                             )
                         ),
                ),
                tabPanel("Volcano",
                         #uiOutput("fs_volcano"),
                         uiOutput("fs_volcano_fig") %>% withSpinner(type=4),
                         
                         div(
                             dropdown(
                                 sliderTextInput("fs_volcano_p",
                                                 label = "Select P threshold:",
                                                 choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                 selected=0.05, grid=T, force_edges=T),
                                 sliderInput("fs_volcano_Stat",
                                             label = "Select |Stat| threshold:",
                                             min=0, max=5, step=0.1, value=0)

                                 ,
                                 size = "xs",
                                 icon = icon("gear", class = "opt"),
                                 up = TRUE
                             )
                         ),
                )
            )
        }
        else if (rv$mode == "List of genes"){
            tabsetPanel(
                tabPanel("Table",
                         dataTableOutput("single_gl_tbl") %>% withSpinner(type=4),
                         
                         div(
                             dropdown(
                                 sliderInput("df_gl_p", "Select P cutoff:", min = 0.01, max = 1,
                                             value = 1, step = 0.01),
                                 sliderInput("df_gl_q", "Select FDR cutoff:", min = 0.01, max = 1,
                                             value = 1, step = 0.01),
                                 sliderInput("df_gl_Stat", "Select |Stat| cutoff:", min = 0, max = 5,
                                             value = 0, step = 0.25)
                                 ,
                                 size = "xs",
                                 icon = icon("gear", class = "opt"),
                                 up = TRUE
                             )
                         ),
                         div(
                             dropdown(
                                 downloadButton("downloadgldf","Download table"),
                                 downloadButton("downloadRNK","Download RNK")
                                 ,
                                 size = "xs",
                                 icon = icon("download", class = "opt"),
                                 up = TRUE
                             )
                         ),
                         
                         
                ),
                tabPanel("Volcano",
                         uiOutput("gl_volcano_fig"),
                         
                         div(
                             dropdown(
                                 sliderTextInput("gl_volcano_p",
                                                 label = "Select P threshold:",
                                                 choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                                 selected=0.05, grid=T, force_edges=T),
                                 sliderInput("gl_volcano_Stat",
                                             label = "Select |Stat| threshold:",
                                             min=0, max=5, step=0.1, value=0)
                                 
                                 ,
                                 size = "xs",
                                 icon = icon("gear", class = "opt"),
                                 up = TRUE
                             )
                         ),
                ),
                tabPanel("Bar", 
                         
                         uiOutput("gl_bar_fig"),
                         
                         div(
                             dropdown(
                                 radioButtons(
                                     inputId = "p1_bar_data",
                                     label = "Data to plot in bar graph:",
                                     choices = rv$gl_cols, #can select any column except gene name
                                     selected = rv$gl_cols[[1]]
                                 ),
                                 radioButtons(
                                     inputId = "p1_bar_sig",
                                     label = "Color by significance:",
                                     choices = c("PValue","FDR"),
                                     selected = "PValue"
                                 )
                                 
                                 ,
                                 size = "xs",
                                 icon = icon("gear", class = "opt"),
                                 up = TRUE
                             )
                         ),
                )
            )
        }
        
    })
    
    ####================= SINGLE VISUALIZATIONS =====================####
    
    
    ####--------------------Table------------------------####
    
    # download current df
    output$downloaddf <- downloadHandler(
        filename = function() {
            paste("data-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            df <- rv$df
            df <- subset(df, PValue <= rv$df_x_p)
            df <- subset(df, FDR <= rv$df_x_q)
            df <- subset(df, abs(Stat) >= rv$df_x_Stat)
            names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
            output_file <- df
            write.csv(output_file, file, 
                      row.names = FALSE, quote=FALSE)})
    
    # download rnk 
    output$downloadRNK <- downloadHandler(
        filename = function() {
            paste("RNK-", rv$show_df, "-", Sys.Date(), ".rnk", sep="")},
        content = function(file) {
            output_file <- rv$df
            output_file <- append_rank(output_file)
            output_file <- output_file[,c("X","Rank")]
            write.table(output_file, file, 
                        row.names=FALSE, quote=FALSE,sep="\t")})
    
    
    
    # show table
    output$single_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        df <- rv$df
        df <- subset(df, PValue <= rv$df_x_p)
        df <- subset(df, FDR <= rv$df_x_q)
        df <- subset(df, abs(Stat) >= rv$df_x_Stat)
        
        names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df
    })
    
    
    # download current gl df
    output$downloadgldf <- downloadHandler(
        filename = function() {
            paste("subset-", rv$show_df, "-", Sys.Date(), ".csv", sep="")},
        content = function(file) {
            df <- rv$df
            df <- subset(df, PValue <= rv$df_gl_p)
            df <- subset(df, FDR <= rv$df_gl_q)
            df <- subset(df, abs(Stat) >= rv$df_gl_Stat)
            names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
            output_file <- df
            write.csv(output_file, file, 
                      row.names = FALSE, quote=FALSE)})
    
    
    # show gl table
    output$single_gl_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        df <- rv$df
        df <- subset(df, PValue <= rv$df_gl_p)
        df <- subset(df, FDR <= rv$df_gl_q)
        df <- subset(df, abs(Stat) >= rv$df_gl_Stat)
        
        names(df)[names(df) == 'Stat'] <- rv$tt[[rv$x_i]]
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df
    })
    
    
    
    ####-----------------single fs volcano ----------------####
    
    # plot area
    output$fs_volcano_fig <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df)

        plotlyOutput("p1_fs_volcano",
                     width = "100%",height = "600px") %>% withSpinner(type=4)
        })
    
    # volcano graph 
    output$p1_fs_volcano <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)

        
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- ifelse(df_p$PValue <rv$fs_volcano_p & abs(df_p$Stat)>rv$fs_volcano_Stat, "red", "gray")
        
        fs_volcano <- plot_ly(
            data = df_p, 
            x = df_p$Stat,
            y = -log10(df_p$PValue),
            mode = 'markers', marker = list(color = df_p$color),
            hoverinfo="text",
            text=c(paste(df_p$X, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3))
                         ))
        )
        
        fs_volcano <- fs_volcano %>% layout(title = paste0("Volcano plot of ", rv$show_df, " (n=",nrow(df_p),")"),
                                            yaxis = list(zeroline = T, title="-log10(PValue)",
                                                         range=c(0,rv$volcano_ymax)),
                                            xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]], 
                                                         range=c(-rv$volcano_xmax,rv$volcano_xmax)))
        
        return(fs_volcano)
    })

    
    ####-----------------single gl bar plot----------------####
    

    
    output$gl_bar_fig <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df)

        #req(input$gl_plottype=='Bar plot')
        plotlyOutput(
            outputId = "p1_bar",
            width = "100%",
            height = "400px"
        )
    })
    
    # bar graph
    output$p1_bar <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)
        
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- -log10(as.numeric(df_p[[rv$p1_bar_sig]]))
        print(head(df_p))
        fig <- plot_ly(
            x = df_p$X,
            y = df_p[[rv$p1_bar_data]],
            name = "Expression bar plot",
            type = "bar",
            hoverinfo="text",
            marker = list(
                colorscale=cscale,
                color = df_p$color,
                colorbar=list(title=paste0("-log10(",rv$p1_bar_sig, ")")),
                cauto = F,cmin = 0,cmax = 3
            ),
            text=c(paste(df_p$X, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3))))
            
            
        )
        return(fig)
    })
    
    ####------------------single gl volcano---------------------####
    
    
    # dynamic ui for volcano
    output$gl_volcano_fig <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df)

        plotlyOutput("p1_gl_volcano",
            width = "100%",height = "400px"
    )})
    
    # volcano graph ################## revise
    output$p1_gl_volcano <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(rv$df)
 
        df_p <- rv$df
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        df_p$color <- ifelse(df_p$PValue <rv$gl_volcano_p & abs(df_p$Stat)>rv$gl_volcano_Stat, "red", "gray")
        
        gl_volcano <- plot_ly(
            data = df_p, 
            x = df_p$Stat,
            y = -log10(df_p$PValue),
            mode = 'markers', marker = list(color = df_p$color),
            hoverinfo="text",
            text=c(paste(df_p$X, 
                         "<br>",rv$tt[[rv$x_i]],":", as.character(round(df_p$Stat, 3)),
                         "<br>PValue:", as.character(round(df_p$PValue, 3)),
                         "<br>FDR:", as.character(round(df_p$FDR, 3)))
                   )
            )
        gl_volcano <- gl_volcano %>% layout(title = paste0("Volcano plot of subset of ", rv$show_df, " (n=",nrow(df_p),")"),
                                            yaxis = list(zeroline = T, title="-log10(PValue)",
                                                         range=c(0, rv$volcano_ymax)),
                                            xaxis = list(zeroline = T, title=rv$tt[[rv$x_i]],
                                                         range=c(-rv$volcano_xmax,rv$volcano_xmax)))
        return(gl_volcano)
    })
    

    
    
    #======================================================================#
    ####                         TWO WAY                                ####
    #======================================================================#
    
    ####---------------------- Events ---------------------------####
    
    # update params
    observe({
        if(is.null(input$xy_p)==F){rv$xy_p <- input$xy_p}
        if(is.null(input$xy_q)==F){rv$xy_q <- input$xy_q}
        if(is.null(input$xy_Stat)==F){rv$xy_Stat <- input$xy_Stat}
        
        if(is.null(input$xy_sc_p)==F){rv$xy_sc_p <- input$xy_sc_p}
        if(is.null(input$xy_sc_q)==F){rv$xy_sc_q <- input$xy_sc_q}
        if(is.null(input$xy_sc_Stat)==F){rv$xy_sc_Stat <- input$xy_sc_Stat}
        if(is.null(input$xy_colormode)==F){rv$xy_colormode <- input$xy_colormode}
        if(is.null(input$xy_sig)==F){rv$xy_sig <- input$xy_sig}
    })
    
    # initialize
    observeEvent(input$xy_confirm, {
        req(is.null(input$selected_x)==F)
        req(is.null(input$selected_y)==F)
        req(length(rv$ll)>=1)
        
        
        withProgress(message = 'Updating data...', value = 0, {
            
            df_x <- isolate(rv$gg[[match(input$selected_x, rv$ll)]])
            incProgress(0.2)
            df_y <- isolate(rv$gg[[match(input$selected_y, rv$ll)]])
            incProgress(0.2)
            df_xy <- merge(df_x,df_y, by = "X")
            incProgress(0.2)
            
            # initialize params
            rv$xyx_i <- isolate(match(input$selected_x, rv$ll))
            rv$xyy_i <- isolate(match(input$selected_y, rv$ll))
            
            rv$xy_p <- 0.25
            rv$xy_q <- 0.25
            rv$xy_Stat <- 0.25
            
            rv$selected_x <- isolate(input$selected_x)
            rv$selected_y <- isolate(input$selected_y)
            
            rv$xy_sc_p <- 0.25
            rv$xy_sc_q <- 0.25
            rv$xy_sc_Stat <- 0
            rv$xy_colormode <- "None"
            rv$xy_sig <- "PValue"
            
            incProgress(0.2)
        })
        rv$df_xy <- df_xy
        
        
    })
    
    
    # observe and pull out shared cols and rows among input dfs (as lists)
    observe({
        req(is.null(input$selected_x)==F)
        req(is.null(input$selected_y)==F)
        req(length(rv$ll)>=1)
        
        # observe shared columns
        xcols <- colnames(rv$gg[[match(input$selected_x,rv$ll)]][-1])
        ycols <- colnames(rv$gg[[match(input$selected_y,rv$ll)]][-1])
        rv$xy_sharedcols <- intersect(xcols, ycols)
        
        xrows <- rv$gg[[match(input$selected_x,rv$ll)]]$X
        yrows <- rv$gg[[match(input$selected_y,rv$ll)]]$X
        rv$xy_sharedrows <- intersect(xrows, yrows)
    })
    
    
    ####---------------------- Sidebar ---------------------------####
    
    # select x and y
    output$select_x <- renderUI({
        req(length(rv$ll) >= 1)
        selectInput(
            inputId = "selected_x",
            label = "Select dataset 1:",
            choices = rv$ll)
    })
    output$select_y <- renderUI({
        req(length(rv$ll) >= 1)
        selectInput(
            inputId = "selected_y",
            label = "Select dataset 2:",
            choices = rv$ll)
    })
    
    # feedback on whether the data has enough shared rows/cols
    output$xy_shared_cols <- renderText({
        req(length(rv$ll) >= 1)
        req(is.null(rv$xy_sharedcols)==F)
        req(is.null(rv$xy_sharedrows)==F)

        if (length(rv$xy_sharedcols)>=1){msg=" (ok)"}
        else{ msg=""}
        
        paste0("Shared columns: ",length(rv$xy_sharedcols), msg)
    })
    output$xy_shared_rows <- renderText({
        req(length(rv$ll) >= 1)
        req(is.null(rv$xy_sharedcols)==F)
        req(is.null(rv$xy_sharedrows)==F)
        
        if (length(rv$xy_sharedrows)>=1){msg=" (ok)"}
        else{ msg=""}
        
        paste0("Shared rows: ",length(rv$xy_sharedrows), msg)
    })
    
    
    
    
    # confirm and submit
    output$xy_confirm <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$xy_sharedcols>=1)
        req(rv$xy_sharedrows>=1)
        
        actionButton("xy_confirm", "Visualize!")
    })
    
    
    ####----------------------Main Tabs---------------------------####
    
    output$xy_main <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df_xy)
        
        tabsetPanel(
            tabPanel("Scatter",
                     
                     
                     plotlyOutput("df_xy_scatter",height = "600px") %>% withSpinner(type=4),
                     
                     div(
                         dropdown(
                             uiOutput("xy_sc_cutoffs"),
                             "Note: This will be applied to all columns."
                             ,
                             size = "xs",
                             icon = icon("cut", class = "opt"),
                             up = TRUE
                         )
                     ),
                     div(
                         dropdown(
                             uiOutput("xy_colormode"),
                             uiOutput("xy_sig"),
                             uiOutput("xy_thresh")
                             ,
                             size = "xs",
                             icon = icon("gear", class = "opt"),
                             up = TRUE
                         )
                     ),
                     div(
                         dropdown(
                             downloadButton("scatter_xy_dl", "Download plot")
                             ,
                             size = "xs",
                             icon = icon("download", class = "opt"),
                             up = TRUE
                         )
                     ),
                     
                     
                     
                     tags$hr(style="border-color: grey;"),
                     strong(textOutput("corline_xy")),br(),
                     verbatimTextOutput("fit_xy_summary")
                     
            ),
            tabPanel("Table",
                     
                     dataTableOutput("df_xy_tbl") %>% withSpinner(type=4),
                     
                     div(
                         dropdown(
                             sliderTextInput("xy_p",
                                             label = "Select P cutoff:",
                                             choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                             selected=0.25, grid=T, force_edges=T),
                             sliderTextInput("xy_q",
                                             label = "Select FDR cutoff:",
                                             choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                             selected=0.25, grid=T, force_edges=T),
                             sliderInput("xy_Stat",
                                         label = "Select |Stat| cutoff:",
                                         min=0, max=5, step=0.1, value=0),
                             "Note: This will be applied to all columns."
                             ,
                             size = "xs",
                             icon = icon("cut", class = "opt"),
                             up = TRUE
                         )
                     ),
                     div(
                         dropdown(
                             downloadButton("xy_tbl_dl",
                                            label = "Download Table")
                             ,
                             size = "xs",
                             icon = icon("download", class = "opt"),
                             up = TRUE
                         )
                     ),
            )
        )
        
    })
    
    
    
    ####================= TWO WAY VISUALIZATIONS =====================####
    
    ####--------------- table -------------------####
    
    # show df_xy table
    output$df_xy_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df_xy)
        
        df_xy <- rv$df_xy
        # cutoffs:
        df_xy <- df_xy %>% filter(PValue.x < rv$xy_p & PValue.y < rv$xy_p)
        df_xy <- df_xy %>% filter(FDR.x < rv$xy_q & FDR.y < rv$xy_q)
        df_xy <- df_xy %>% filter(abs(Stat.x) > rv$xy_Stat & abs(Stat.y) > rv$xy_Stat)
        
        # to replace the stat col names 
        colnames(df_xy) <- gsub("Stat", rv$tt[[rv$xyx_i]], colnames(df_xy))
        
        
        # to abbreviate the long column names...take first 5 letters
        colnames(df_xy) <- sapply(names(df_xy), function(x){
            if (nchar(x)>5) 
            {return (paste0(substr(x, start = 1, stop = 5),"..."))}
            else{return (x)}
        })
        
        # to round everything down to 3 decimals
        df_xy[-1] <- df_xy[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df_xy
    }, options = list(headerCallback= JS("function(thead, data, start, end, display){",
                                         sprintf("  var tooltips = [%s];", toString(paste0("'", colnames(rv$df_xy), "'"))),
                                         "  for(var i = 1; i <= tooltips.length; i++){",
                                         "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                         "  }",
                                         "}")))
    
    
    ####--------------- scatter plot + corr -------------------####
    
    #------------------ Param -----------------#
    
    # cutoffs
    output$xy_sc_cutoffs <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df_xy)
        fluidRow(
            box(width = 12,
                sliderTextInput("xy_sc_p",
                                label = "Select P cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=0.25, grid=T, force_edges=T),
                sliderTextInput("xy_sc_q",
                                label = "Select FDR cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=0.25, grid=T, force_edges=T),
                sliderInput("xy_sc_Stat",
                            label = "Select |Stat| cutoff:",
                            min=0, max=5, step=0.1, value=0)
            )
        )
    })
    
    # color mode
    output$xy_colormode <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df_xy)
        radioButtons(
            inputId = "xy_colormode",
            label = "Represent significance by:",
            choices = c("None", "Two colors", "Color and size"))
    })
    output$xy_sig <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df_xy)
        req(rv$xy_colormode !="None")
        radioButtons(
            inputId = "xy_sig",
            label = "Significance:",
            choices = c("PValue", "FDR"),
            selected="PValue")
    })
    output$xy_thresh <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$xy_colormode =="Two colors")
        sliderTextInput("xy_thresh",
                        label = "Select threshold:",
                        choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                        selected=0.05, grid=T, force_edges=T)
    })
    

    
    #------------------ main -----------------#
    
    # plotly scatter
    output$df_xy_scatter <- renderPlotly({
        req(length(rv$ll) >= 1)
        req(is.null(rv$df_xy)==F)
        req(is.null(rv$xy_colormode)==F)

        df_p <- rv$df_xy
        df_p[df_p==0]<-0.00001 # replace 0 with 0.001
        # cutoffs
        df_p <- df_p %>% filter(PValue.x < rv$xy_sc_p & PValue.y < rv$xy_sc_p)
        df_p <- df_p %>% filter(FDR.x < rv$xy_sc_q & FDR.y < rv$xy_sc_q)
        df_p <- df_p %>% filter(abs(Stat.x) > rv$xy_sc_Stat & abs(Stat.y) > rv$xy_sc_Stat)
        
        xsig <- paste0(rv$xy_sig,".x")
        ysig <- paste0(rv$xy_sig,".y")
        
        # initialize marker settings as none
        df_p$color <- "black"
        df_p$color <- as.factor(df_p$color)
        df_p$size <- 5
        marker_settings <- list(
            color= df_p$color, size= df_p$size, 
            line = list(color = 'white', width = 0))
        
        
        if (rv$xy_colormode== "Two colors"){ # is this a good idea????
            #print(head(df_p))
            
            df_p$color <- as.character(df_p$color)
            df_p$color[which(df_p[[xsig]] < input$xy_thresh | df_p[[ysig]] < input$xy_thresh)] <- "red"
            df_p$color <- as.factor(df_p$color)
            
            df_p$size <- 5
            marker_settings <- list(
                color= df_p$color, size= df_p$size, 
                line = list(color = 'white', width = 1))
        }
        else if (rv$xy_colormode== "Color and size"){
            df_p[df_p==0]<-0.00001 # replace 0 with 0.001
            df_p$color <- -log10(as.numeric(df_p[[xsig]]))
            df_p$color <- as.numeric(df_p$color)
            df_p$size <- -log10(as.numeric(df_p[[ysig]]))*2+3
            #print(head(df_p))
            marker_settings <- list(
                color= df_p$color, size= df_p$size,
                opacity=.7, line = list(color = 'white', width = 1),
                colorscale=cscale, cauto=F, cmin=0, cmax=3,
                colorbar=list(title=paste0('-log10(PValue(X))')))
        }
        # calculate cor line
        rv$fit_xy <- lm(Stat.x ~ Stat.y, df_p)
        
        fig <- plot_ly(
            data = df_p, 
            x = df_p$Stat.x,
            y = df_p$Stat.y,
            type = 'scatter',
            mode = 'markers', 
            marker = marker_settings,
            hoverinfo="text",
            text=c(paste(df_p$X, 
                         "<br>",rv$tt[[rv$xyx_i]],"(x):", round(df_p$Stat.x, 3),
                         "<br>p(x):", round(df_p$PValue.x, 3),
                         ", q(x):", round(df_p$FDR.x, 3),
                         "<br>",rv$tt[[rv$xyy_i]],"(y):", round(df_p$Stat.y, 3),
                         "<br>p(y):", round(df_p$PValue.y, 3),
                         ", q(y):", round(df_p$FDR.y, 3)
                         ))
        )
        fig <- fig %>% layout(title = paste0(rv$selected_x, " vs ", rv$selected_y, " (n=",nrow(df_p),")"),
                                            yaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyx_i]],"(x)")),
                                            xaxis = list(zeroline = T, title=paste0(rv$tt[[rv$xyy_i]],"(y)"))
                              )
        rv$plt <- fig
        return(fig)
    })
    

    # correlation line
    output$corline_xy <- renderText({
        req(length(rv$ll) >= 1)
        req(rv$fit_xy)
        intercept = format(round(coef(rv$fit_xy)[[1]], 2), nsmall = 2)
        slope = format(round(coef(rv$fit_xy)[[2]], 2), nsmall = 2)
        r2= format(round(summary(rv$fit_xy)$r.squared, 2), nsmall = 2)
        return (paste0("Correlation line: y = ", slope,"x + ",intercept, ", R^2 = ", r2))
    })
    output$fit_xy_summary <- renderPrint({
        req(length(rv$ll) >= 1)
        req(rv$fit_xy)
        summary(rv$fit_xy)
    })
    
    # download plotly html graph
    output$scatter_xy_dl <- downloadHandler(
        filename = function() {paste("scatter-", Sys.Date(), ".html", sep = "")},
        content = function(file) {saveWidget(as_widget(rv$plt), file, selfcontained = TRUE)})
    
    # download table
    output$xy_tbl_dl <- downloadHandler(
        filename = function() {paste0("data-",Sys.Date(),"-",input$selected_x,"_vs_",input$selected_y,".csv")},
        content = function(file) {
            df_xy <- rv$df_xy
            df_xy <- df_xy %>% filter(PValue.x < rv$xy_p & PValue.y < rv$xy_p)
            df_xy <- df_xy %>% filter(FDR.x < rv$xy_q & FDR.y < rv$xy_q)
            df_xy <- df_xy %>% filter(abs(Stat.x) > rv$xy_Stat & abs(Stat.y) > rv$xy_Stat)
            output_file <- df_xy
            write.csv(output_file, file, 
                      row.names = FALSE, quote=FALSE)})
    
    
    #======================================================================#
    ####                        MULTIPLE WAY                            ####
    #======================================================================#

    
    ####---------------------- Events ---------------------------####
    
    # update these into rv when selections change
    observeEvent(input$n_cutoff_by,{rv$n_cutoff_by<-input$n_cutoff_by})
    observeEvent(input$p_cutoff_2,{rv$p_cutoff_2<-input$p_cutoff_2})
    observeEvent(input$q_cutoff_2,{rv$q_cutoff_2<-input$q_cutoff_2})
    observeEvent(input$Stat_cutoff_2,{rv$Stat_cutoff_2<-input$Stat_cutoff_2})
    observeEvent(input$n_to_plot,{rv$n_to_plot<-input$n_to_plot})
    observeEvent(input$heatmap_sortby,{rv$heatmap_sortby<-input$heatmap_sortby})
    
    observe({
        if(is.null(input$n_p)==F){ rv$n_p <- input$n_p }
        if(is.null(input$n_q)==F){ rv$n_q <- input$n_q }
        if(is.null(input$n_Stat)==F){ rv$n_Stat <- input$n_Stat }
    })
    
    
    # observe and pull out shared cols and rows among input dfs (as lists)
    observe({
        if (length(rv$heatmap_i)>=2){
            # observe shared columns
            colns <- lapply(rv$heatmap_i, function(x){
                colnames(rv$gg[[x]][-1]) # exclude genename column
            })
            rv$n_sharedcols <- Reduce(intersect, colns)
            
            # observe shared rows
            rowns <- lapply(rv$heatmap_i, function(x){
                rv$gg[[x]]$X 
            })
            rv$n_sharedrows <- Reduce(intersect, rowns)
        }
        else{
            rv$n_sharedcols <- NULL
            rv$n_sharedrows <- NULL
        }
    })
    
    # observe and match selected df index into new list
    observeEvent(input$heatmap_dfs,{
        rv$heatmap_i <- input$heatmap_dfs
        rv$heatmap_i <- lapply(rv$heatmap_i,function(x){match(x,rv$ll)})
    })
    
    
    
    
    # observe selection and combine to data table
    observeEvent(input$n_use_data,{
        req(length(rv$ll) >= 1)
        
        withProgress(message = 'Updating data...', value = 0, {
            
            
            # make list of dfs 
            data <- lapply(rv$heatmap_i, function(x){
                Stat<-rv$gg[[x]]
                return (Stat)
            }) 
            incProgress(0.2)
            
            # annotate the non-name columns in each df in list with filename
            data <- lapply(seq_along(data), function(x, y){
                colnames(data[[x]])[-1] <- paste(colnames(data[[x]])[-1],y[[x]], sep="_")
                return (data[[x]])}
                , y=input$heatmap_dfs)
            incProgress(0.2)
            
            # merge list of dfs on gene
            plotdata <- Reduce(function(x,y) merge(x,y, by = "X"), data) 
            incProgress(0.2)
            
            # initialize params
            rv$nx_i <- isolate(rv$heatmap_i)[[1]]
            
            rv$n_p <- 1
            rv$n_q <- 1
            rv$n_Stat <- 0
            
            rv$iso_graph_dfs<-isolate(input$heatmap_dfs)
            rv$iso_sharedcols<- isolate(rv$n_sharedcols)
            
            rv$n_cutoff_by <- isolate(input$heatmap_dfs[[1]])
            rv$p_cutoff_2 <- 0.05
            rv$q_cutoff_2 <- 1
            rv$Stat_cutoff_2 <- 0
            rv$n_to_plot <- isolate(rv$n_sharedcols[[1]])
            rv$heatmap_sortby <- isolate(input$heatmap_dfs[[1]])
            
            
            incProgress(0.2)
            
            plotdata <- remove_nas(plotdata)
        
        })
        rv$df_n <- plotdata
    })
    
    
    ####---------------------- Sidebar ---------------------------####
    
    # select data
    output$select_df_p2 <- renderUI({
        req(length(rv$ll) >= 1)
        checkboxGroupInput(
            inputId = "heatmap_dfs",
            label= shiny::HTML("Select from uploaded datasets: 
                               <span style='color: gray'>(2 or more required)</span>"),
            choices = rv$ll)
    })
    
    # feedback on whether the data has enough shared rows/cols
    output$n_shared_cols <- renderText({
        req(length(rv$ll) >= 1)
        req(length(input$heatmap_dfs) >= 2)
        if (length(rv$n_sharedcols)>=1){msg=" (ok)"}
        else{ msg=""}
        
        paste0("Shared columns: ",length(rv$n_sharedcols), msg)
    })
    output$n_shared_rows <- renderText({
        req(length(rv$ll) >= 1)
        req(length(input$heatmap_dfs) >= 2)
        if (length(rv$n_sharedrows)>=1){msg=" (ok)"}
        else{ msg=""}
        paste0("Shared rows: ",length(rv$n_sharedrows), msg)
    })
    
    output$n_use_data <- renderUI({
        req(length(rv$ll) >= 1)
        req(length(rv$heatmap_i) > 1)
        req(length(rv$n_sharedrows)>=1)
        req(length(rv$n_sharedcols)>=1)
        actionButton("n_use_data", "Visualize!")
    })
    
    ####----------------------Main Tabs---------------------------####
    
    output$n_main <- renderUI({
        req(rv$df_n)
        
        tabsetPanel(
            tabPanel("Heatmap",br(),
                     
                     
                     uiOutput("n_heatmap") %>% withSpinner(type=4),
                     div(
                         dropdown(
                             uiOutput("select_sortby_p2"),
                             uiOutput("n_to_plot"),
                             uiOutput("cutoff_by_p2"),
                             uiOutput("n_cutoffs"),
                             
                             uiOutput("plot_heatmap_p2"),
                             
                             size = "xs",
                             icon = icon("gear", class = "opt"),
                             up = TRUE
                         )
                     )
                     
            ),
            tabPanel("Table",
                     
                     dataTableOutput("df_n_tbl") %>% withSpinner(type=4),
                     div(
                         dropdown(
                             
                             uiOutput("n_tbl_cutoffs"),
                             "Note: This will be applied to all columns.",
                             
                             size = "xs",
                             icon = icon("cut", class = "opt"),
                             up = TRUE
                         )
                     )
                     
            )
        )
    })
    
    ####================= MULTIPLE VISUALIZATIONS =====================####
    
    ####-------------------- table ------------------------####
    
    
    output$n_tbl_cutoffs <- renderUI({
        req(rv$df_n)
        fluidRow(
            box(width = 12, 
                sliderTextInput("n_p",
                                label = "Select P cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=rv$n_p, grid=T, force_edges=T),
                sliderTextInput("n_q",
                                label = "Select FDR cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=rv$n_q, grid=T, force_edges=T),
                sliderInput("n_Stat",
                            label = "Select |Stat| cutoff:",
                            min=0, max=5, step=0.1, value=rv$n_Stat)
            )
        )
    })
    
    
    output$df_n_tbl <- DT::renderDataTable({
        req(length(rv$ll) >= 1)
        req(rv$df_n)
        
        df <- rv$df_n
        
        # some cutoff here
        df <- df %>% mutate(df$m = do.call(pmax, select(df, contains("PValue_")))) %>%
            filter(m < rv$n_p)
        df <- df %>% mutate(df$m = do.call(pmax, select(df, contains("FDR_")))) %>% 
            filter(m < rv$n_q)
        df <- df %>% mutate(df$m = do.call(pmin, abs(select(df, contains("Stat"))))) %>% 
            filter(m > rv$n_Stat)
        df <- df[1:(length(df)-1)] # delete last helper column
        
        # to replace the stat col names 
        colnames(df) <- gsub("Stat", rv$tt[[rv$nx_i]], colnames(df))
        
        # to abbreviate the long column names...take first 5 letters
        colnames(df) <- sapply(names(df), function(x){
            if (nchar(x)>5) 
            {return (paste0(substr(x, start = 1, stop = 5),"..."))}
            else{return (x)}
        })
        
        # to round everything down to 3 decimals
        df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
        
        df
        
    }, options = list(headerCallback= JS("function(thead, data, start, end, display){",
                                         sprintf("  var tooltips = [%s];", toString(paste0("'", colnames(rv$df_n), "'"))),
                                         "  for(var i = 1; i <= tooltips.length; i++){",
                                         "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                         "  }",
                                         "}")))
    
    
    
    ####--------------------multiple deg heatmap------------------------####
    
    #-------------- param -------------------#
    
    
    
    # sort by which dataset
    output$select_sortby_p2 <- renderUI({
        req(rv$df_n)
        radioButtons(
            inputId = "heatmap_sortby",
            label= "Sort heatmap by:",
            choices = rv$iso_graph_dfs,
            selected = rv$select_sortby_p2
            )
    })
    
    # which data column to plot (i.e. Stat, etc)
    output$n_to_plot <- renderUI({
        req(rv$df_n)
        selectInput(
            inputId = "n_to_plot",
            label= shiny::HTML("Plot data: 
                               <span style='color: gray'>(Note: only shared columns are selectable)</span>"),
            choices = rv$iso_sharedcols, # this displays all the shared columns, 
            selected = rv$n_to_plot
            )
    })
    
    output$cutoff_by_p2 <- renderUI({
        req(rv$df_n)
        selectInput(
            inputId = "n_cutoff_by",
            label= "Cutoff by dataset:",
            choices = rv$iso_graph_dfs,
            selected = rv$cutoff_by_p2
            )
    })
    
    output$n_cutoffs <- renderUI({
        req(rv$df_n)
        fluidRow(
            box(width = 12, 
                sliderTextInput("p_cutoff_2",
                                label = "Select P cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=rv$p_cutoff_2, grid=T, force_edges=T),
                sliderTextInput("q_cutoff_2",
                                label = "Select FDR cutoff:",
                                choices= c(0.001,0.01,0.05,0.1,0.15,0.2,0.25,0.3,0.5,1),
                                selected=rv$q_cutoff_2, grid=T, force_edges=T),
                sliderInput("Stat_cutoff_2",
                            label = "Select |Stat| cutoff:",
                            min=0, max=5, step=0.1, value=rv$Stat_cutoff_2)
            )
        )
    })

    
    
    
    
    
    
    output$n_heatmap <- renderUI({
        req(length(rv$ll) >= 1)
        req(rv$df_n)
        # req(input$draw_heatmap)
        req(is.null(heatmap)==F)
        
        plotOutput(
            outputId = "heatmap", width = "100%", height = "600px"
        ) %>% withSpinner(type=4)
    })
    
    output$heatmap = renderPlot({
        req(length(rv$ll) >= 1)
        req(rv$df_n)
        # req(input$draw_heatmap)
        
        
        
        plotted <- rv$df_n
        # some p/ q cutoff here
        selected <- paste0("PValue_",rv$n_cutoff_by)
        plotted <- plotted %>% filter(!!sym(selected) < rv$p_cutoff_2)
        selected <- paste0("FDR_",rv$n_cutoff_by)
        plotted <- plotted %>% filter(!!sym(selected) < rv$q_cutoff_2)
        selected <- paste0("Stat_",rv$n_cutoff_by)
        plotted <- plotted %>% filter(abs(!!sym(selected)) > rv$Stat_cutoff_2)
        
        rownames(plotted) <- plotted$X # put genename as index
        plotted <- select(plotted,contains(rv$n_to_plot)) # only extract wanted columns to plot
        
        req(nrow(plotted) != 0)
        
        #colnames(plotted) <- input$heatmap_dfs # name columns after analysis # rename columns (optional)
        plotted <- plotted[order(-plotted[paste(rv$n_to_plot,"_", rv$heatmap_sortby, sep="")]),] # sort in descending order based on selected column
        
        # to replace the stat col names 
        colnames(plotted) <- gsub("Stat", rv$tt[[rv$nx_i]], colnames(plotted))
        
        hmplot <- pheatmap(plotted, 
                      #scale = "row", # don't use z score
                      cluster_rows=F, cluster_cols=T,
                      show_rownames=F, show_colnames=T) 
        return (hmplot)
    })
    
    
   

    ####--------XXXXXXXXXXXXXXXXXXXXXXXXXXXXXX---------####
    
    
    # ####-------------------- Info ------------------------####
    # output$text1 <- renderUI({
    #     if(input$More_info=="Introduction"){
    #         includeHTML("inc/introduction.html")
    #     } else if(input$More_info=="Information"){
    #         includeHTML("inc/information.html")
    #     } else if(input$More_info=="Help"){
    #         includeHTML("inc/help.html")
    #     } else if(input$More_info=="References"){
    #         includeHTML("inc/references.html")
    #     }
    # })
    
    ####------------------- DEBUG -----------------------####
    
    # # DEBUG (single)
    # output$odataset <- renderPrint({
    #     paste("rv mode"," = ", rv$mode, ", ",
    #           "rv_genelist"," = ", rv$genelist, ", ",
    #           "submit_genelist"," = ", input$submit_genelist, ", ",
    #           "Table index"," = ", current(), ",",
    #           "input = ", input$file$name, ",",
    #           "ll size = ", length(rv$ll),
    #           "gg size = ", length(rv$gg))
    # })
    # output$debug2 <- renderPrint({
    #     paste("n_use_data = ", input$n_use_data, ", ",
    #           "heatmap_i = ", rv$heatmap_i, ",",
    #           "cutoff_by = ", rv$n_cutoff_by, ",",
    #           "p_cutoff = ", rv$p_cutoff_2, ",",
    #           "ll size = ", length(rv$ll), ",",
    #           "gg size = ", length(rv$gg))
    # })
    
    
    
    
}
# ===================================================== server.R END ============

# div(
#     dropdown(
#         downloadButton(outputId = "down_box_5", label = "Download plot"),
#         size = "xs",
#         icon = icon("download", class = "opt"), 
#         up = TRUE
#     )
# ),
