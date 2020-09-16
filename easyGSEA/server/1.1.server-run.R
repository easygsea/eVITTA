# RNK help --------------
    observeEvent(input$q1,{
        showModal(modalDialog(
            inputId = "rank_md",
            title = "Ranked list file format (*.rnk)",
             # includeHTML(paste0(getwd(),"/inc/rnk_explanation.html")),
            # dataTableOutput('example_data1'),
            includeMarkdown(paste0(getwd(),"/inc/rnk_explaination.md")),
            # includeMarkdown(knitr::knit(paste0(getwd(),"/inc/rnk_explaination.Rmd"),quiet=T)),
            easyClose = TRUE,size="l"
            # , footer = modalButton("Close")
        ))
    })

  output$example1 <- renderTable({(example_data1 <- read.csv(paste0(getwd(),"/inc/cel2_example1.rnk"),header = TRUE, sep = "\t"))},escape = FALSE)
  output$example2 <- renderTable({(example_data2 <- read.csv(paste0(getwd(),"/inc/cel2_example2.csv")))},escape = TRUE)

    # --------------  1.1 select databases --------------------
    
    # disable selection when user confirms gmts; enables upon modify
    # this is to prevent accidentally messing up selections by changing species
    observe({
        req(is.null(rv$db_status)==F)
        if (rv$db_status == "selected"){
            shinyjs::disable("selected_species")
        }
        else if (rv$db_status == "modify"){
            shinyjs::enable("selected_species")
        }
    })

    # --------------  1.2 select GMTs ---------------------------
    
    observe({
        req(nchar(input$selected_species)>0)
        req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
        
        species <- input$selected_species

        for(collection in sort(names(gmt_collections_paths[[species]]))){
          if(collection %in% col_f){
            f = FALSE
          }else{
            f = TRUE
          }
            # print(paste0(species,gsub(" ","_",collection)))
            rv$v[[species]][[collection]] <- column(
              6,
              box(
                title = strsplit(collection,"_")[[1]][[2]], width = 12, status = "primary", collapsible = T, collapsed = f,
                checkboxGroupInput(
                  inputId = paste0(species,gsub(" ","_",collection)),
                  label = NULL,
                  # label = strsplit(collection,"_")[[1]][[2]],
                  choices = sort(gmt_collections[[species]][[collection]]),
                  selected = gmt_collections_selected[[species]][[collection]]
                )
              )
            )
        }
    })
    
    output$test_db <- renderUI({
        req(nchar(input$selected_species)>0)
        req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
        
        species = input$selected_species
        
        fluidRow(
            column(
                width = 12,
    #             tags$script(HTML('$(document).ready(function(){
    #   $("#showdbs_collapsible").on("hide.bs.collapse", function(){
    #     $("#showdbs").html("<span class=\\\"glyphicon glyphicon-collapse-down\\\"></span> Advanced database options ...");
    #   });
    #   $("#showdbs_collapsible").on("show.bs.collapse", function(){
    #     $("#showdbs").html("<span class=\\\"glyphicon glyphicon-collapse-up\\\"></span> Advanced database options ...");
    #   });
    # });')),
                actionButton("showdbs", "Advanced database options ...", 
                         icon = icon("book-open")
                         # icon = icon("collapse-down", lib = "glyphicon")
                         # ,style = "default"
                         # , type = "toggle"
                         # #, class = "btn-primary btn-sm"
                         # ,`data-toggle`="collapse"
                         # ,`data-target` = "#showdbs_collapsible"
                         ),
                br(),br(),
                # conditionalPanel('input.showdbs % 2 == 1', id="showdbs_panel",
                #                  rv$v[[species]],
                #                  uiOutput("bs_reset_db")
                # ),
                # br()
            )
        )
    })
    
    # --------------  1.2.2 show databases in a modal ---------------------------
    observeEvent(input$showdbs,{
      species = input$selected_species
      
      showModal(modalDialog(id = "db_modal",
        title = HTML(paste0("Available databases for <i>",species_translate(species),"</i>")),
        div(
          fluidRow(
            # "Hello"
            rv$v[[species]]
          ),
          fluidRow(
            column(12,align="right",
                   uiOutput("bs_reset_db")
            )
          )
        ),
        easyClose = F,size="m"
        , footer = tagList(
          # modalButton('Cancel'), 
          bsButton('select_db', h4('Select to continue!'), style = "primary", block=TRUE)
        )
      ))
    })
    
    # reset to default selected databases
    output$bs_reset_db <- renderUI({
      req(input$selected_species != "")
      req(is.null(rv$db_status)==T || rv$db_status == "modify")
      bsButton(
        inputId = "reset_db", 
        label = "Reset to default selections",
        # style = "primary",
        type = "button")
    })
    
    # observe modal "select" bsbutton, dismiss modal
    observeEvent(input$select_db,{
      dbs = NULL; rv$db_modal = "yes"
      species<-input$selected_species
      
      for(collection in sort(names(gmt_collections_paths[[species]]))){
        db_id = paste0(species,gsub(" ","_",collection))
        # db_name = input[[db_id]]
        dbs = c(dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% isolate(input[[db_id]]))])
      }
      
      if(length(dbs)<1){
        showNotification("Select at least one database", type = "error", duration = 3)
      }else{
        removeModal()
      }
    })
    
    #-------------- 1.3 select GMTs ----------------
    
    # write selected databases into RV
    observeEvent(input$add_db, {
        rv$dbs = NULL
        
        species<-input$selected_species
        
        if(is.null(rv$db_modal)){
          for(collection in sort(names(gmt_collections_paths[[species]]))){
            db_id = paste0(species,gsub(" ","_",collection))
            rv$dbs = c(rv$dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% gmt_collections_selected[[species]][[collection]])])
          }
        }else{
          for(collection in sort(names(gmt_collections_paths[[species]]))){
            db_id = paste0(species,gsub(" ","_",collection))
            rv$dbs = c(rv$dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% input[[db_id]])])
          }
        }
        # for(collection in sort(names(gmt_collections_paths[[species]]))){
        #     db_id = paste0(species,gsub(" ","_",collection))
        #     # db_name = input[[db_id]]
        #     if(is.null(input[[db_id]])){
        #       rv$dbs = c(rv$dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% gmt_collections_selected[[species]][[collection]])])
        #     }else{
        #       rv$dbs = c(rv$dbs,gmt_collections[[species]][[collection]][which(gmt_collections[[species]][[collection]] %in% input[[db_id]])])
        #     }
        # }
        
        rv$db_status <- "selected"
    })
    
    # reset species, at the same time reset rnk/glist
    observeEvent(input$add_db_modify, {
      rv$db_status <- "modify"
      
      # clear RVs
      # rv$run = NULL
      rv$glist_check = NULL
      rv$rnk_check = NULL
      rv$infile_check = NULL
      rv$example_file = NULL
      rv$infile_confirm = NULL
      rv$infile_name = NULL
      rv$infile_path = NULL
      rv$file_upload_status = NULL
      rv$rnk_or_deg = NULL
      rv$gene_lists_mat = NULL
      rv$db_modal = NULL
      
      # rest glist UIs
      shinyjs::reset("gene_list")
      shinyjs::enable("gene_list")
      shinyjs::reset("glist_name")
      shinyjs::enable("glist_name")
      
      # reset gsea UIs
      shinyjs::reset("rnkfile")
      shinyjs::enable("rnkfile")  
        
    })
    
    # add / modify button
    output$bs_add_db <- renderUI({
        req(input$selected_species != "")
        if ((is.null(rv$db_status)==TRUE) || (rv$db_status=="modify")){
          fluidRow(
            column(
              width = 12,
              bsButton(
                inputId = "add_db", 
                label = "Confirm selection",
                style = "primary",
                type = "button"),
              br(),br()
              )
          )
        }
        else if (rv$db_status == "selected"){
          fluidRow(
            column(
              width = 12,
              bsButton(
                inputId = "add_db_modify", 
                label = "Modify selection",
                style = "default",
                type = "button"),
              br(),br()
            )
          )
        }
        
    })
    
    # reset button
    observeEvent(input$reset_db, {
        rv$run = NULL
        
        species <- input$selected_species
        for(collection in names(gmt_collections_paths[[species]])){
          sel = gmt_collections_selected[[species]][[collection]]
          if(is.null(sel)){
            updateCheckboxGroupInput(session,
                                     inputId = paste0(species,gsub(" ","_",collection)),
                                     selected = ""
            )
          }else{
            updateCheckboxGroupInput(session,
                                     inputId = paste0(species,gsub(" ","_",collection)),
                                     selected = sel
            )
          }
            
        }
    })
    

# ------------ 2.1.1 Upload & reset RNK ---------------
    # UI file input
    output$ui_rnk <- renderUI({
        req(input$selected_mode == "gsea")
        # req(rv$db_status == "selected")
        div(

      
            fileInput("rnkfile",
                      label = p("3. Upload RNK file:",
                                tags$style(type = "text/css", "#q1 {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;}"),
                                bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")),
                      buttonLabel = "Upload...",
                      accept = c(
                          "text/tab-separated-values",
                          "text/comma-separated-values",
                          ".csv",".txt",".tab",".tsv",
                          ".rnk")
                      
                      

            ),
            bsTooltip("q1", "DEG file also supported. Click to learn more", placement = "top")
            
            
        )

    })
    
    # UI reset
    output$bs_file_reset <- renderUI({
        req(input$selected_mode == "gsea")
        req(is.null(rv$infile_name)==F)
        fluidRow(column(12,
          bsButton(
            inputId = "reset",
            label = "Reset file",
            style = "default",
            type = "button"),
        ))
        
    })

    
    # reset RNK input widget
    observeEvent(input$reset, {
      reset_rnk()
    })
    
    # read in RNK file path name, disable widget
    observeEvent(input$rnkfile, {
        rv$file_upload_status = "uploaded"
        rv$infile_name = input$rnkfile$name
        rv$infile_path = input$rnkfile$datapath
        shinyjs::disable("rnkfile")
    })

# ------------- 2.1.2 select corresponding table columns -----------------
    # UI: select columns to 
    # output$feedbacks <- renderUI
    observe({
      req(input$selected_mode == "gsea")
      req(rv$db_status == "selected")
      req(rv$file_upload_status == "uploaded")
      req(is.null(rv$infile_confirm) == T)
      
      showModal(modalDialog(
        title = "Select corresponding columns to continue",
        
        fluidRow(
          column(12,
            wellPanel(
              # shiny::HTML("<p style='font-style:italic'>Select corresponding columns</p>"),
              h4("Select corresponding columns"),
              uiOutput("feedback_filecontent_deg"),
              uiOutput("feedback_filecontent_rnk"),
              # uiOutput("feedback_filecontent_confirm"),
              style = paste0("background:",bcol3)
            )
          ),
          column(12,
                 p("Your query file content:"),
                 uiOutput("feedback_filecontent")
          )
        ),,
          
        easyClose = F,
        footer = bsButton(
          "filecontent_confirm",
          h4("Confirm and continue!"),
          block = TRUE,
          style = "primary"
        )
      ))
      
    })
        
# ------------- 2.1.3 Upload and reset example RNK/DE --------------
    observeEvent(input$loadExampleRNK,{
        rv$example_file = NULL
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
          updateRadioButtons(
            session,
            "gene_identifier",
            selected = "symbol"
          )
            sampleRNK_file <- paste0(getwd(),"/inc/",input$selected_species,".rnk")
            rv$infile_name = paste0(input$selected_species,".rnk")
            rv$infile_path = sampleRNK_file
            shinyjs::reset("rnkfile")
            shinyjs::disable("rnkfile")
            removeModal()
            rv$infile_check = NULL
            rv$example_file = "yes"
            rv$file_upload_status = "uploaded"
        }
    })
    
    observeEvent(input$loadExampleDE,{
        rv$example_file = NULL
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
          updateRadioButtons(
            session,
            "gene_identifier",
            "2. Gene identifier",
            choices = gene_identifiers,
            selected = "symbol",
            inline = TRUE
          )
            sampleDE_file <- paste0(getwd(),"/inc/",input$selected_species,".csv")
            rv$infile_name = paste0(input$selected_species,".csv")
            rv$infile_path = sampleDE_file
            shinyjs::reset("rnkfile")
            shinyjs::disable("rnkfile")
            removeModal()
            rv$infile_check = NULL
            rv$example_file = "yes"
            rv$file_upload_status = "uploaded"
        }
    })

    
# ----------------- 2.1.4 check and store input file content into rv$data_head -----------------------
    observe({
        req(is.null(rv$infile_name)==F)
        rv$infile_check=NULL
        rv$input_symbol = NULL
        rv$gene_lists_mat = NULL
        rv$run = NULL
        
        rv$rnkll <- strsplit(isolate(rv$infile_name),"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1] # add value to rv
        ranks <- read_delim(isolate(rv$infile_path), ",")# , escape_double = FALSE, trim_ws = TRUE)

        # print(str(head(ranks)))
        if(ncol(ranks)==1){
            ranks <- read_delim(isolate(rv$infile_path), "\t")# , escape_double = FALSE, trim_ws = TRUE)
        }
        ranks =  ranks %>% #[complete.cases(ranks), ]
          dplyr::select_if(~sum(!is.na(.)) > 0)

        # detect if RNK or DEG
        if(ncol(ranks)==2){
            rv$rnk_or_deg = "rnk"
        }else if(ncol(ranks)>2){
            rv$rnk_or_deg = "deg"
        }else{
            showNotification("You uploaded a file with < 2 columns. Please click the help button for accepted file formats.",type = "error",duration = 3)
        }
        
        # save had data into RV
        rv$data_head = ranks
    })
    
    # ----------------- 2.1.5 Return RNK -----------------------
    observeEvent(input$filecontent_confirm,{
      removeModal()
        data = rv$data_head
        wtext = tags$b(
          "Duplicated genes found in your uploaded file. Only the first duplicate(s) will be kept. Do you want to continue?",
          style = "color: #FA5858;"
        )
        
        if(ncol(data)==2){
          if(is.null(input$rank_column) && is.null(input$gene_column)){
            shinyalert("Please select the rank and the gene columns.")
          }else if(is.null(input$rank_column)){
            shinyalert("Please select the rank column.")
          }else if(is.null(input$gene_column)){
            shinyalert("Please select the gene column.")
          }else{
            all_genes = data[[input$gene_column]]
            duplicates = duplicated(all_genes)

            if(TRUE %in% duplicates){
              shinyWidgets::ask_confirmation(
                inputId = "confirm_duplicate_rnk",
                title = NULL,
                text = wtext,
                type = "warning",
                btn_labels = c("Cancel", "Continue"),
                btn_colors = c("#00BFFF", "#FE2E2E"),
                html = TRUE
              )
            }else{
              convert_rnk()
              return_rnk()
            }
          }
        }else if(ncol(data)>2){
          if(is.null(input$gene_column) && is.null(input$logfc_column) && is.null(input$p_column)){
            shinyalert("Please select the gene, the logFC, and the p-value columns.")
          }else if(is.null(input$gene_column) && is.null(input$logfc_column)){
            shinyalert("Please select the gene and the logFC columns.")
          }else if(is.null(input$gene_column) && is.null(input$p_column)){
            shinyalert("Please select the gene and the p-value columns.")
          }else if(is.null(input$logfc_column) && is.null(input$p_column)){
            shinyalert("Please select the logFC and the p-value columns.")
          }else if(is.null(input$gene_column)){
            shinyalert("Please select the gene column.")
          }else if(is.null(input$logfc_column)){
            shinyalert("Please select the logFC column.")
          }else if(is.null(input$p_column)){
            shinyalert("Please select the p-value column.")
          }else{
            all_genes = data[[input$gene_column]]
            duplicates = duplicated(all_genes)
            
            if(TRUE %in% duplicates){
              shinyWidgets::ask_confirmation(
                inputId = "confirm_duplicate_deg",
                title = NULL,
                text = wtext,
                type = "warning",
                btn_labels = c("Cancel", "Continue"),
                btn_colors = c("#00BFFF", "#FE2E2E"),
                html = TRUE
              )
            }else{
              convert_rnk_from_deg()
              return_rnk()
              
            }
          }
        }
    })

    # ----------------- 2.1.6 check if duplicated genes -----------------------
    
    observeEvent(input$confirm_duplicate_rnk,{
      if(input$confirm_duplicate_rnk){
        convert_rnk()
        return_rnk()
      }else{
        reset_rnk()
      }
    },ignoreInit = T)
    
    observeEvent(input$confirm_duplicate_deg,{
      if(input$confirm_duplicate_deg==TRUE){
        convert_rnk_from_deg()
        return_rnk()
      }
    },ignoreInit = T)
    
#====================================================#
######      2.2.1 GList mode: input gene lists  ######
#====================================================#
    output$ui_glist <- renderUI({
        req(input$selected_mode == "glist")
        # req(input$selected_species != "")
        # req(is.null(rv$dbs)==F)
      fluidRow(
        column(
          width = 12,
          bsTooltip("gene_list_q", "Input newline-delimited gene list", placement = "top"),
          textAreaInput(
            inputId = "gene_list",
            label = p("3. Input your genes (",
                      tags$style(type = "text/css", "#load_example_glist {display: inline-block;height: 20px;padding: 0;vertical-align: baseline;}"),
                      add_help("gene_list_q", style="font-size:medium;padding:3px 0 0 0;position:absolute;right:0.8em;"),
                      actionLink("load_example_glist", label = tags$u("example data")
                                 
                                 ),
                      "):"
            ),
            placeholder = "Paste your genes here ...",
            height = 110
          )
        ),
        column(
          width = 6,
          textInput(
            "glist_name",
            NULL,
            placeholder = 'Name your list ...'
          )
        ),
        column(
          width = 2, #offset = 6,
          bsButton(
            inputId = "gene_list_clear",
            label = "Reset",
            style = "default"
          )
        ),
        column(
          width = 4, align="right",
          uiOutput("glist_add_button")
        )
        
      )
    })
    
    # Glist add button
    output$glist_add_button <- renderUI({
      req(is.null(rv$gene_lists)|is.null(rv$glist_check))
      bsButton(
        inputId = "gene_list_add",
        label = "Submit",
        style = "primary"
      )
    })
    
    #---------------- 2.2.2 read in GList-----------------
    # from input field
    observeEvent(input$gene_list_add,{
        species = isolate(input$selected_species)  
        # print(input$gene_list)
        if(nchar(species)>2){
            if(input$gene_list != ""){
                shinyjs::disable("gene_list")
                shinyjs::disable("glist_name")
                
                genelist = as.character(input$gene_list)
                genelist = gsub("\"","",genelist)
                genelist = strsplit(genelist,"\n")
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*,\\s*')))
                genelist = unlist(lapply(genelist, function(x) strsplit(x,'\\s*;\\s*')))
                genelist = unlist(strsplit(genelist," "))
                genelist = unique(genelist)
                
                if(is.null(genelist)==F){
                    # save original gene lists into RV
                    rv$gene_lists = genelist
                    
                    # autodetect and convert into SYMBOL (if other/mixed identifier) using gprofiler2
                    if(input$gene_identifier == "other"){
                      withProgress(message = "Autodetecting and converting gene IDs...",{
                        Sys.sleep(0.1)
                        incProgress(1)
                        lst = convert_gene_id(species,genelist)
                        
                        if(is.null(lst)){
                          # no ID detected in database
                          rv$glist_check = "none"
                        }else{
                          # check percentage of IDs found in database
                          g_perc = lst[[1]]
                          
                          # if <30%, reports error
                          if(g_perc < 0.5){
                            rv$glist_check = "low"
                          }else{
                            rv$glist_check = "pass"
                          }
                          
                          # convert ID and save converted IDs & conversion table into RVs
                          rv$gene_lists_after = lst[[2]]
                          rv$gene_lists_mat = lst[[3]]
                          
                        }
                      })
                    }else{
                      rv$gene_lists_after = rv$gene_lists
                      rv$glist_check = "pass"
                    }

                    # name the analysis
                    if(input$glist_name == ""){
                        rv$rnkll = "unnamed"
                    }else{
                        rv$rnkll = input$glist_name
                    }
                }
            }else{
                showNotification("Please input your query. Click example data for a trial run.",type="error",duration=3)
            }
        }else{
            showNotification("Please select species that matches your query.",type="error",duration=3)
        }
        # }else{
        #     rv$gene_lists = rv$data_glist
        # 
        #     # successfully submitted
        #     rv$glist_status = "submitted"
        # }
    })
    
    # -------------- 2.2.3 clear GList input ---------------------------
    observeEvent(input$gene_list_clear, {
        # rv$run = NULL
        
        rv$glist_check = NULL
        rv$gene_lists = NULL
        rv$gene_lists_after = NULL
        
        shinyjs::reset("gene_list")
        shinyjs::enable("gene_list")
        shinyjs::reset("glist_name")
        shinyjs::enable("glist_name")
        # updateTextAreaInput(session,
        #                     inputId = "gene_list",
        #                     value = "",
        #                     placeholder = "Type to input.."
        # )
    })
    
    #----------- 2.2.4 Example GList --------------
    observeEvent(input$load_example_glist,{
        if(input$selected_species == ""){
            showNotification("Please select your species of interest.",type="error",duration=2)
        }else{
            updateTextAreaInput(session,
                                inputId = "gene_list",
                                value = gsub(";","\n",glist_example[input$selected_species][[1]])
            )
        }
    })
    

    

#---------- 3. run parameters & confirm buttons ---------
    # clear Glist rv when switching to gsea mode
    observe({
        req(input$selected_mode == "gsea")
        rv$gene_lists = NULL
        rv$gene_lists_after = NULL
    })
    
    # UI GSEA parameter
    output$ui_gsea_par <- renderUI({
      req(rv$db_status == "selected")
      
      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
        
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
        
      }
      
      fluidRow(
        box(
          width = 12, title = "Advanced run parameters", status = "primary", collapsible = T, collapsed = T,
          wellPanel(
            # h4("Run parameters"),
            splitLayout(
              numericInput("mymin", 
                           HTML(paste0("Min:",
                                       add_help("mymin_q")))
                           ,rv$gmin),
              numericInput("mymax", 
                           HTML(paste0("Max:",
                                       add_help("mymax_q")))
                           ,rv$gmax),
              uiOutput("ui_nperm")
            )
            ,style = "background:#e6f4fc;"
          ),
          bsTooltip("mymin_q", "Minimum gene set size", placement = "top"),
          bsTooltip("mymax_q", "Maximum gene set size", placement = "top")
        )
      )
    })

    output$ui_nperm <- renderUI({
        req(input$selected_mode == "gsea")
      div(
        numericInput("nperm", 
                     HTML(paste0("# perm:",add_help("nperm_q")))
                     ,rv$gperm),
        bsTooltip("nperm_q", "No. of permutations", placement = "top")
      )
    })
    
    # UI confirm GSEA
    output$run_GSEA <- renderUI({
      req(input$selected_mode == "gsea")
      req(rv$db_status == "selected")
      
      req(is.null(rv$rnk_check)==F)
      req(is.null(rv$rnkgg)==F)
        
      actionBttn("confirm1", 
                 h4(span(icon("play-circle"),"RUN GSEA!")),
               style="simple", color="primary", # size = "large",
               block = TRUE
               )
    })
    
    # UI confirm GList
    output$run_GList <- renderUI({
      req(input$selected_mode == "glist")
      req(rv$db_status == "selected")
      
      req(is.null(rv$glist_check)==F)
      req(is.null(rv$gene_lists_after)==F)

      actionBttn("confirm2", 
               h4(span(icon("play-circle"),"RUN ORA!")),
               style="simple", color="primary", # size = "large",
               block = TRUE)
        
    })

    
    #===============================================#
    #####           4.1 run GSEA!!!             #####
    #===============================================#
    # runs upon the analysis name is provided.
    observeEvent(input$confirm1, {
        
        rv$run_mode = "gsea"
        ranks = rv$rnkgg
        names(ranks) = toupper(names(ranks))
        
        sd = sd(ranks); rv$sd_high = sd * 2.5
        
        if(is.null(input$mymin)==F){rv$gmin=input$mymin}
        if(is.null(input$mymax)==F){rv$gmax=input$mymax}
        if(is.null(input$nperm)==F){rv$gperm=input$nperm}
        
        # reset RVs
        reset_rvs()

        rv$bar_pathway = rv$dbs
        rv$bubble_pathway = rv$dbs
        rv$volcano_pathway = rv$dbs
        
        species <- isolate(input$selected_species)
        
        withProgress(message = "Running GSEA analysis...",value = 0.2, {

            # ------ read GMTs into list ------ #
            # gmts are temporarily stored in list and will be looped over during gsea run.
            
            # initialize
            gmts=list()
            catnames=list()
            
            for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(rv$db_modal)==F){
                  inputs = input[[db_id]]
                }else{
                  inputs = gmt_collections_selected[[species]][[collection]]
                }
                    
                for(cat_name in inputs){
                  gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]
                  m_list <- gmtPathways(gmt_path)
                  
                  gmts = c(gmts,list(m_list))
                  catnames <- c(catnames, cat_name)
                  incProgress(0.1)
                }
            }
            
            # save GMTs to rv
            rv$gmts = unlist(gmts,recursive = F)

            # ------ run fgsea ------ #
            errors = 0
            for (i in seq_along(gmts)){
                # uppercase genes
                m_list = lapply(gmts[[i]], function(x) toupper(x))
                
                a_lens = lengths(m_list)
                              
                if(max(a_lens)<rv$gmin || min(a_lens)>rv$gmax){errors = errors + 1}
                
                frun <- try(fgseaRes <- fgsea(pathways = m_list,
                                  stats    = ranks,
                                  minSize  = rv$gmin,
                                  maxSize  = rv$gmax,
                                  nperm = rv$gperm))
                
                if(inherits(frun, "try-error")) {        
                  errors = errors + 1
                }else{
                  db <- rep(catnames[[i]], nrow(fgseaRes))
                  fgseaRes <- cbind(db,fgseaRes)
                  #print(head(fgseaRes))
                  rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
                  # rv$fgseagg <- c(rv$fgseagg, list(fgseaRes))
                  rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.01&fgseaRes$ES>0,na.rm=TRUE)
                  rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES>0,na.rm=TRUE)
                  rv$no_down_01 = rv$no_down_01 + sum(fgseaRes$padj<0.01&fgseaRes$ES<0,na.rm=TRUE)
                  rv$no_down_05 = rv$no_down_05 + sum(fgseaRes$padj<0.05&fgseaRes$ES<0,na.rm=TRUE)
                  # rv$fgseagg <- c(rv$fgseagg, list(catnames[[i]] = fgseaRes))
                  incProgress(0.2)
                }
            }
            
            if(errors > 0 && nrow(rv$fgseagg)<1){
              db_selected = names(rv$dbs)
              db_selected = paste(db_selected,collapse = "; ")
              # ErrorMessage <- conditionMessage(attr(frun, "condition"))  # the error message
              #show a modal dialog if there is an error reading files causing crash
              showModal(modalDialog(
                title = h3(HTML("Please click and adjust <b>Advanced run parameters</b>")),
                tags$li(h4(paste0("Database(s): ",db_selected))),
                tags$li(h4(paste0("No gene sets available after filtering by min=",rv$gmin
                                  ," and max=",rv$gmax))),
                
                size = "l",
                easyClose = TRUE
              ))
            }else{
              # count number of filtered GSs in GMTs
              l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
              rv$gmts_length = sum(l)
              incProgress(0.1)
              
              # determine if success or warnings
              if(nrow(rv$fgseagg)>0){
                rv$run = "success"
              } else {
                rv$run = "failed"
              }
              incProgress(0.1)
            }
        })
    })
    
    #===============================================#
    #####             4.2 run GList!!!          #####
    #===============================================#
    
    observeEvent(input$confirm2, {
        rv$run_mode = "glist"
        
        # reset RVs
        reset_rvs()

        # read in parameters
        
        genelist = toupper(rv$gene_lists_after)

        if(is.null(input$mymin)==F){rv$gmin=input$mymin}
        if(is.null(input$mymax)==F){rv$gmax=input$mymax}

        rv$bar_pathway = rv$dbs
        rv$bubble_pathway = rv$dbs

        species <- isolate(input$selected_species)

        withProgress(message = "Running overrepresentation analysis...",value = 0.2, {
            
            # ------ read GMTs into list ------ #
            # gmts are temporarily stored in list and will be looped over during gsea run.
            
            # initialize
            gmts=list()
            catnames=list()
            all_genes=list()
            
            for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(rv$db_modal)==F){
                  inputs = input[[db_id]]
                }else{
                  inputs = gmt_collections_selected[[species]][[collection]]
                }
                
                for(cat_name in inputs){
                  gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]
                  m_list <- gmtPathways(gmt_path)

                  # get all genes
                  a_genes = toupper(unname(unlist(m_list,recursive = T))) %>% unique(.)
                  all_genes <- c(all_genes, list(a_genes))
                  
                  gmts = c(gmts,list(m_list))
                  catnames <- c(catnames, cat_name)
                  incProgress(0.1)
                }
                
            }

            # save GMTs to rv
            rv$gmts = unlist(gmts,recursive = F)

            # ------ run fgsea ------ #
            errors = 0
            for (i in seq_along(gmts)){
                # uppercase genes
                m_list = lapply(gmts[[i]], function(x) toupper(x))
                
                # genes present in the database
                in_genes = genelist[genelist %in% all_genes[[i]]]

                if(identical(in_genes,character(0))){incProgress(0.2);next}
                
                frun <- try(fgseaRes <- fora(pathways = m_list,
                                  genes    = in_genes,
                                  universe = all_genes[[i]],
                                  minSize  = rv$gmin,
                                  maxSize  = rv$gmax
                                  ))
                
                if(inherits(frun, "try-error")) {        
                  errors = errors + 1
                }else{
                  db <- rep(catnames[[i]], nrow(fgseaRes))
                  fgseaRes <- cbind(db,fgseaRes)
                  rv$fgseagg <- rbind(rv$fgseagg, fgseaRes)
                  rv$no_up_01 = rv$no_up_01 + sum(fgseaRes$padj<0.01,na.rm=TRUE)
                  rv$no_up_05 = rv$no_up_05 + sum(fgseaRes$padj<0.05,na.rm=TRUE)
                  incProgress(0.2)
                }
            }
            
            if(errors > 0 && is.null(rv$fgseagg)){
              db_selected = names(rv$dbs)
              db_selected = paste(db_selected,collapse = "; ")
              # ErrorMessage <- conditionMessage(attr(frun, "condition"))  # the error message
              #show a modal dialog if there is an error reading files causing crash
              showModal(modalDialog(
                title = h3(HTML("Please click and adjust <b>Advanced run parameters</b>")),
                tags$li(h4(paste0("Database(s): ",db_selected))),
                tags$li(h4(paste0("No gene sets available after filtering by min=",rv$gmin
                                  ," and max=",rv$gmax))),

                size = "l",
                easyClose = TRUE
              ))
            }else{
              # count number of filtered GSs in GMTs
              l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
              rv$gmts_length = sum(l)
              incProgress(0.1)
              
              # determine if success or warnings
              if(is.null(rv$fgseagg)==F && nrow(rv$fgseagg)>0){
                rv$run = "success"
              } else {
                rv$run = "failed"
              }
              incProgress(0.1)
            }
            

        })
    })