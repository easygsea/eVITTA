# # DEMO SESSION code -------------------------------------------------------
# 
# library(later)
# # the modal to remind the user it is a demo session
# observe({
#   init_demo_gsea()
#   #init_demo_ora()
#   showModal(modalDialog(title = tags$h3("Welcome to easyGSEA demo session"),
#                         tags$h4("Explore the sample output that performs interactively in the same way as real output.")
#                         ,br()
#                         ,tags$h4("Click OK to follow the intro tour."),
#                         size = "m",
#                         easyClose = FALSE
#                         ,footer = actionButton("welcome_modal",label = "OK")))
# 
# })
# # when the user closed the modal, start rintrojs
# observeEvent(input$welcome_modal, {
#   removeModal()
#   rv$demo_yes <- "yes"
#   if(rv$demo_mode == "gsea"){
#     call_introjs(rbind(intros$R_pre,intros$R_post_with_conversion_table,intros$R_post))
#   }else {
#     call_introjs(rbind(intros$R_pre_ora,intros$R_post_with_conversion_table_ora,intros$R_post_ora))
#   }
# })
# 
# # start rintrojs when users switch tabs
# observeEvent(input$tabs,{
#   if(input$tabs == "kegg"){
#     later(~call_introjs(rbind(intros$ER_post,intros$ER_post_with_pathway)), 0.1)
#   } else if(input$tabs == "network"){
#     later(~call_introjs(intros$EN_post), 3)
#   } else if(input$tabs == "download"){
#     later(~call_introjs(intros$D_post), 2)
#   } else {
# 
#   }
# })
# # when user switch tabs, call introjs
# observeEvent(input$plot_type, {
#   if(input$plot_type != "bar")
#     later(~call_introjs(rbind(intros$ER_post,intros$ER_post_with_pathway)), 0.1)
# })
# # END--------------------------------------------------------------------------------

# RNK help --------------
    observeEvent(input$q1,{
        showModal(modalDialog(
            inputId = "rank_md",
            title = "Ranked list file format (*.rnk) for pre-ranked GSEA runs",
             # includeHTML(paste0(getwd(),"/inc/rnk_explanation.html")),
            # dataTableOutput('example_data1'),
            includeMarkdown(paste0(getwd(),"/inc/rnk_explaination.md")),
            # includeMarkdown(knitr::knit(paste0(getwd(),"/inc/rnk_explaination.Rmd"),quiet=T)),
            easyClose = TRUE,size="l"
            , footer = modalButton("OK")
        ))
    })

  output$example1 <- renderTable({(example_data1 <- read.csv(paste0(getwd(),"/inc/cel2_example1.rnk"),header = TRUE, sep = "\t"))},escape = FALSE)
  output$example2 <- renderTable({(example_data2 <- read.csv(paste0(getwd(),"/inc/cel2_example2.csv")))},escape = TRUE)

  # Navigation button to next tab -------------
  output$nav_btn_run <- renderUI({
    req(rv$run == "success" & rv$demo_mode == "")

    div(
      nav_btn_f("gsea_f")
      ,bsTooltip("gsea_f", HTML("Proceed to <b>Enrichment Results</b>"))
    )
  })

  observeEvent(input$gsea_f,{
    updateTabItems(session, "tabs","kegg")
  })


    # --------------  1.2 select GMTs ---------------------------

    # observe({
    #     req(nchar(input$selected_species)>0 && input$selected_species != "other")
    #     req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
    # 
    #     species <- input$selected_species
    # 
    #     for(collection in sort(names(gmt_collections_paths[[species]]))){
    #       if(collection %in% col_f){
    #         f = FALSE
    #       }else{
    #         f = TRUE
    #       }
    #         # print(paste0(species,gsub(" ","_",collection)))
    #         rv$v[[species]][[collection]] <- column(
    #           6,
    #           box(
    #             title = strsplit(collection,"_")[[1]][[2]], width = 12, status = "primary", collapsible = T, collapsed = f,
    #             checkboxGroupInput(
    #               inputId = paste0(species,gsub(" ","_",collection)),
    #               label = NULL,
    #               # label = strsplit(collection,"_")[[1]][[2]],
    #               choices = sort(gmt_collections[[species]][[collection]]),
    #               selected = gmt_collections_selected[[species]][[collection]]
    #             )
    #           )
    #         )
    #     }
    # })

    output$test_db <- renderUI({
    
        req(nchar(input$selected_species)>0  && input$selected_species != "other")
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
    bsTooltip("showdbs","Default selection: Biological processes and pathways. <i>Click</i> for more options."
              ,placement = "top"),
                br(),br(),
                # conditionalPanel('input.showdbs % 2 == 1', id="showdbs_panel",
                #                  rv$v[[species]],
                #                  uiOutput("bs_reset_db")
                # ),
                # br()
            )
        )
    })
    
    # specify the upload limits here (in byte)
    batch_mb_limit <- 300*(1024^2)
    total_mb_limit <- 300*(1024^2)

    # -------------- 1.2b upload GMT -------------------
    output$gmt_upload <- renderUI({
      # initialize RVs for our demo session
      if(rv$demo_mode == "gsea"){
        init_demo_gsea()
      }
      if(rv$demo_mode == "ora"){
        init_demo_ora()
      }
      
      req(input$selected_species == "other")
      req(is.null(rv$db_status)==T || rv$db_status == "modify")

      div(#class="box__input",id="drop-area", align="center",
          div(class="form-group shiny-input-container",#id="drag_gmt",
              div(class="input-group",
                  tags$label(class="input-group-btn input-group-prepend",
                    HTML('<span id="gmt_cc" style="width: 100%;" class="btn btn-default btn-file">')
                    ,HTML('<img src="upload.jpg" width="18%" class="mx-2"><br>Drag <b>GMT</b> file(s) here or click to browse') #<div style="font-weight:400;line-height:200%;">
                    ,HTML('
        <input id="gmt_c" name="gmt_c" type="file" style="display: none;" multiple="multiple" accept="text/tab-separated-values,.txt,.tab,.tsv,.gmt"/>
    ')
                  )
                  ,HTML('</span>')
                  )

              ,div(id="gmt_c_progress", class="progress progress-striped active shiny-file-input-progress",
                   div(class="progress-bar")
              )
          )
          ,bsTooltip("gmt_cc",HTML("Upload your own gene set database file (GMT) for custom analysis")
                     ,placement = "top")
      )

    })
    
    # read in GMTs
    observeEvent(input$gmt_c,{
      # Check the type of file you uploads, if the file is other types, remind the user
      ext_check <- ""
      if(!tools::file_ext(input$gmt_c$name) %in% c(
        'text/tab-separated-values','txt','tab','tsv','gmt'
      )){
        ext_check <- "no"
        shinyjs::reset("gmt_c")
        shinyalert("We only accept files that are .txt,.tab,.tsv,.gmt; 
                   please check your file(s) and re-upload file(s) with the correct file extensions .")
      }
      
      req(ext_check != "no")
      
      if(sum(input$gmt_c$size) >= batch_mb_limit){  
        showModal(modalDialog(
          inputId = "size_reminder_modal1",
          div(
            paste0("The files you uploaded exceed 300 MB, please modify it to proceed. Try to delete unneeded columns and 
            only keep the columns that you are interested in. 
            Then upload your files again. Thank you.")
            ,style="font-size:200%"),
          easyClose = TRUE,size="l"
          # , footer = modalButton("Close")
        ))
        shinyjs::reset("gmt_c")
      }else if ((sum(input$gmt_c$size) + sum(rv$GMTDF$size)) >= total_mb_limit) {
        showModal(modalDialog(
          inputId = "size_reminder_modal2",
          div(
            paste0("You have exceeded your storage limit of 300 MB. Please delete the unneeded files. 
            Then upload your files again. Thank you.")
            ,style="font-size:200%"),
          easyClose = TRUE,size="l"
          # , footer = modalButton("Close")
        ))
        shinyjs::reset("gmt_c")
      }
      
      req(sum(input$gmt_c$size) < batch_mb_limit)
      req((sum(input$gmt_c$size) + sum(rv$GMTDF$size)) < total_mb_limit)
      
      # reset and initialize temporary RVs
      rv$gmt_cs_new <- list()
      rv$gmt_cs_paths_new <- list()
      rv$gmt_temp <- NULL
      
      df <- input$gmt_c
      rv$gmt_temp <- df
      
      gmt_names = list()
      for(i in 1:nrow(df)){
        gmt = df[i,]
        gmt_name_o = gmt$name
        gmt_path = gmt$datapath

        # add a number to the file name is already uploaded
        if(gmt_name_o %in% rv$gmt_cs){
          if(grepl("\\([[:digit:]]+\\)$",gmt_name_o)){
            n <- gsub("(.*)\\(([[:digit:]]+)\\)$", "\\2", gmt_name_o)
            n <- as.numeric(n) + 1
            gmt_name <- gsub("(.*)\\(([[:digit:]]+)\\)$", paste0("\\1(",n,")"), gmt_name_o)
          }else{
            gmt_name <- paste0(gmt_name_o,"(1)")
          }
          
          print(rv$gmt_temp$name)
          # rename the file
          rv$gmt_temp$name[rv$gmt_temp$name == gmt_name_o] <- gmt_name
        }else{
          gmt_name <- gmt_name_o
        }
        
        # write data into corresponding RVs
        rv$gmt_cs_new = c(rv$gmt_cs_new, gmt_name)
        rv$gmt_cs_paths_new = c(rv$gmt_cs_paths_new, gmt_path)
      }
      
      # render a modal after uploads
      showModal(modalDialog(
        id = "gmt_modal",
        title = HTML(paste0("Name your uploaded GMTs ",add_help("gmt_modal_q"))),
        bsTooltip("gmt_modal_q",HTML("Enter a unique abbreviation for each uploaded GMT. easyGSEA recognizes any string before the first occurrence of an underscore (\"_\") as the identifier for each gene set database/library.")
                  ,placement = "right"),
        div(
          materialSwitch(
            inputId = "gmt_name_in_file",
            label = HTML(paste0("Abbreviation(s) already included in uploaded GMT(s)?",add_help("gmt_name_in_file_q"))),
            value = rv$gmt_name_in_file, inline = TRUE, width = "100%",
            status = "danger"
          ),
          bsTooltip("gmt_name_in_file_q",HTML("Click and switch to TRUE if your uploaded GMT(s) already include(s) a database identifier in each gene set name, formated as \"XXX_YYYYY\", where \"XXX\" = the database name, and \"YYYYY\" = the gene set name.")
                    ,placement = "right")
        )
        , uiOutput("ui_name_gmt")
        , easyClose = F,size="m"
        , footer = tagList(
          modalButton('Cancel'),
          bsButton('named_gmt', 'Confirm to proceed', style = "primary", block=F)
        )
      ))
    })
    
    # --------- 1.2b2.manual GMT naming by user ----------
    observeEvent(input$gmt_name_in_file,{rv$gmt_name_in_file <- input$gmt_name_in_file})
    
    output$ui_name_gmt <- renderUI({
      req(!rv$gmt_name_in_file)
      
      div(
        wellPanel(
          style = paste0("background:",bcol3),
          fluidRow(
            lapply(rv$gmt_cs_new, function(x){
              i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
              id <- paste0("GMT",i)
              column(6,
                     textInput(
                       id,
                       label = x,
                       value = id
                     )
              )
            })
            ,uiOutput("ui_highlight")
          )
          
        )
        ,fluidRow(
          column(
            12,
            bsButton(
              "reset_gmt_names",
              "Reset to default naming system"
            )
          )
        )
        ,fluidRow(
          column(
            12,
            tags$hr(style="border: 0.5px dashed black; margin-bottom: 1em;"),
            HTML("<span style='font-size:110%;margin-bottom: 1em;'>Error color codes (box borders):</span>")
          )
          ,column(
            4,
            tags$hr(style="border: 1px solid lightgrey; margin-top: 0.5em; margin-bottom: 0.5em;"),
            p("Valid entry")
          )
          ,column(
            4,
            tags$hr(style="border: 1px solid salmon; margin-top: 0.5em; margin-bottom: 0.5em;"),
            p("Duplicate entries")
          )
          ,column(
            4,
            tags$hr(style="border: 1px solid orange; margin-top: 0.5em; margin-bottom: 0.5em;"),
            p("Entry been used")
          )
          ,column(
            4,
            tags$hr(style="border: 1px solid navy; margin-top: 0.5em; margin-bottom: 0.5em;"),
            p("Empty entry")
          )
          ,column(
            4,
            tags$hr(style="border: 1px solid orchid; margin-top: 0.5em; margin-bottom: 0.5em;"),
            p("Entry contains underscore(s)")
          )
        )
      )
      
    })
    
    # highlight if a textinput has an error
    output$ui_highlight <- renderUI({
      req(is.null(rv$gmt_cs_new) == F)
      
      htag <- c()
      
      # observe if duplicate names entered
      used <- sapply(rv$gmt_cs_new, function(x){
        i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
        id <- paste0("GMT",i)
        input[[id]]
      })
      
      # observe if repetitive name use in the RVs
      used_d <- lapply(str_split(names(rv$gmt_cs), ":", n=2), function(x) x[1]) 
      
      # boxes' parameters
      shadow_w <- "0 0 .5em"
      border_w <- ".5px"
      cl1 <- ""; cl2 <- ""
      
      for(x in rv$gmt_cs_new){
        i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
        id <- paste0("GMT",i)
        
        if(sum(used %in% input[[id]])>1){
          cl1 <- cl2 <- "salmon"
        }else if(input[[id]] %in% used_d){
          cl1 <- cl2 <- "orange"
        }else if(nchar(input[[id]])==0){
          cl1 <- cl2 <-"navy"
        }else if(grepl("_",input[[id]])){
          cl1 <- cl2 <- "orchid"
        }else{
          cl1 <- "none"; cl2 <- "lightgrey"; shadow_w <- ""
        }
        
        # generate the styles
        ss <- sprintf("{box-shadow:%s %s; border:%s solid %s;}",shadow_w,cl1,border_w,cl2)
        htag <- c(htag, paste0("#",id,ss))
      }
      
      tags$head(tags$style(HTML(
        paste0(htag, collapse = " ")
      )))
    })
    
    # reset to default naming system
    observeEvent(input$reset_gmt_names,{
      for(x in rv$gmt_cs_new){
        i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
        id <- paste0("GMT",i)
        updateTextInput(session,
          id, label = x, value = id
        )
      }
    })
    
    # load uploaded GMTs into RV when confirmed
    observeEvent(input$named_gmt,{
      # if GMTs are named manually
      if(!rv$gmt_name_in_file){
        # observe if any entry is empty
        error_0 <- 0
        # observe if repetitive name use in the RVs
        error_d <- 0
        # observe if underscore use in entries
        error_u <- 0
        # check if duplicate names are entered
        used <- c()
        # already-used names
        used_d <- lapply(str_split(names(rv$gmt_cs), ":", n=2), function(x) x[1]) 
        
        for(x in rv$gmt_cs_new){
          i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
          id <- paste0("GMT",i)
          
          if(nchar(input[[id]])==0){
            error_0 <- error_0 + 1
          }else{
            used <- c(used, input[[id]])
            if(input[[id]] %in% used_d){
              error_d <- error_d + 1
            }
            if(grepl("_", input[[id]])){
              error_u <- error_u + 1
            }
          }
        }
        
        # render an error msg if empty entry
        if(error_0 > 0){
          shinyalert("Please enter an abbreviation for each uploaded GMT file.")
        }
        # proceed only if a name is assigned to each GMT
        req(error_0 == 0)
        
        # render an error msg if underscore in entry
        if(error_u > 0){
          shinyalert("Please avoid using underscores (\"_\").")
        }
        # proceed only if no underscore
        req(error_u == 0)
        
        # observe if repetitive name use in the inputs
        error_f <- 0
        
        if(T %in% duplicated(used)){
          error_f <- error_f + 1
          shinyalert("Please enter a unique abbreviation for each uploaded GMT file.")
        }
        
        # proceed only if a name is assigned to each GMT
        req(error_f == 0)
        
        
        # render alert if same names identified
        if(error_d > 0){
          shinyalert("Abbreviations(s) highlighted in orange have been used in a previous upload. Please enter a unique abbreviation for each GMT.")
        }
        
        # proceed only if a name is assigned to each GMT
        req(error_d == 0)
        
        #add new files that are not in df already to the df
        rv$GMTDF<-rbind(rv$GMTDF,rv$gmt_temp[!(rv$gmt_temp$name %in% rv$GMTDF$name)])
        
        #IMPORTANT: GMT seems to assume all uploaded GMTs are unique, so we need to drop duplicates
        
        ids <- sapply(rv$gmt_cs_new, function(x){
          i <- match(x,rv$gmt_cs_new) + length(rv$gmt_cs)
          id <- paste0("GMT",i)
          return(input[[id]])
        })
      }else{
        
      }


      # name the uploaded GMT files
      rv$gmt_cs_new <- ids
      names(rv$gmt_cs_new) <- paste0(ids,":",rv$gmt_cs_new)
      
      # update the core RVs
      rv$gmt_cs <- c(rv$gmt_cs,rv$gmt_cs_new)
      rv$gmt_cs_paths = c(rv$gmt_cs_paths,rv$gmt_cs_paths_new)
      
      # clear temporary rv
      rv$gmt_cs_new <- NULL

      removeModal()
    })

    # --------------  1.2.2 show databases in a modal ---------------------------
    observeEvent(input$showdbs,{
      # req(nchar(input$selected_species)>0 && input$selected_species != "other")
      # req(is.null(rv$db_status)==TRUE || rv$db_status == "modify")
      
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
      div(
        style="display: inline-block;vertical-align:top;",
        bsButton(
          inputId = "reset_db",
          label = "Reset to default selections",
          # style = "primary",
          type = "button")
        ,bsButton(
          inputId = "deselect_db",
          label = "Deselect all",
          # style = "primary",
          type = "button")
        
      )
      
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
        shinyalert("Select at least one database")
      }else{
        removeModal()
      }
    })
    

    #-------------- 1.3 select GMTs ----------------

    # write selected databases into RV
    observeEvent(input$add_db, {
        rv$dbs = NULL

        species<-input$selected_species

        if(species == "other"){
          if(length(rv$gmt_cs)<1){
            shinyalert("Please upload a GMT file to proceed")
          }else{
            rv$db_status <- "selected"
          }
        }else{
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
          rv$db_status <- "selected"
        }
        
        if (is.null(rv$db_status)==F && rv$db_status == "selected"){
          shinyjs::disable("selected_species")
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

    })
    

    # reset species, at the same time reset rnk/glist
    observeEvent(input$add_db_modify, {
      rv$db_status <- "modify"
      shinyjs::enable("selected_species")

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
      rv$gene_lists_mat1 = NULL; rv$gene_lists_mat2 = NULL
      rv$db_modal = NULL
      rv$gmt_cs = NULL

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
                label = "Confirm to proceed",
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

    # reset dbs button
    observeEvent(input$reset_db, {
        rv$run = NULL
        
        rv$glist_check = NULL
        rv$gene_lists = NULL
        rv$gene_lists_after = NULL

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
    
    # deselect dbs button
    observeEvent(input$deselect_db,{
      rv$run = NULL
      
      rv$glist_check = NULL
      rv$gene_lists = NULL
      rv$gene_lists_after = NULL
      
      species <- input$selected_species
      for(collection in names(gmt_collections_paths[[species]])){
        updateCheckboxGroupInput(session,
                                 inputId = paste0(species,gsub(" ","_",collection)),
                                 selected = ""
        )
      }
      
    })


# ------------ 3.1.1 Upload & reset RNK ---------------
    # UI file input
    output$ui_rnk <- renderUI({
        req(input$selected_mode == "gsea")
        # req(rv$db_status == "selected")

      if(input$selected_species == "other"){
        noo = "2"
      }else{
        noo = "3"
      }
        div(id = "upload_file_box",


            fileInput("rnkfile",
                      label = HTML(paste0(noo,". Upload RNK or DEG file:",
                                tags$style(type = "text/css", "#q1 {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline; position: absolute;}"),
                                bsButton("q1", label = "", icon = icon("question"), style = "info", size = "extra-small")
                                )),
                      # buttonLabel = "Upload...",
                      accept = c(
                          "text/tab-separated-values",
                          "text/comma-separated-values",
                          ".csv",".txt",".tab",".tsv",
                          ".rnk")



            ),
            bsTooltip("q1", "Comma- or tab-delimited. <b><i>Click</i></b> to learn more and load our <u>example data</u> for a trial run!", placement = "top")


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
    
    # # onclick of the rnkfile upload button
    # onclick(
    #   "rnkfile", 
    #   if(is.null(rv$db_status) || rv$db_status != "selected"){
    #     shinyalert("Please select the species and corresponding datases")
    #   }
    #   )

    # read in RNK file path name, disable widget
    observeEvent(input$rnkfile, {
      # Check the type of file you uploads, if the file is other types, remind the user
      ext_check <- ""
      if(!tools::file_ext(input$rnkfile$name) %in% c(
        'text/tab-separated-values','txt','tab','tsv', 'csv', 'rnk'
      )){
        ext_check <- "no"
        shinyjs::reset("rnkfile")
        shinyalert("We only accept files that are .txt,.tab,.tsv. 
                   Please click the help button for accepted file formats.")
      }
      
      req(ext_check != "no")
      
        rv$file_upload_status = "uploaded"
        rv$infile_name = input$rnkfile$name
        rv$infile_path = input$rnkfile$datapath
        shinyjs::disable("rnkfile")
        # the modal that appears whent the file user upload exceeds 50MB, Version1
        if(input$rnkfile$size >= 10*1024^2){
          showModal(modalDialog(
            inputId = "size_reminder_modal",
            # title = "The file size exceeds 10MB.",
            div("The file you have uploaded exceeds 10MB. Please delete unneeded columns and
            only keep gene names, log fold changes (logFC), and p values.
            Then press \"reset file\" and upload the trimmed file again. Thank you.",style="font-size:200%"),
            easyClose = TRUE,size="l"
            , footer = modalButton("OK")
          ))
        }
    })

# ------------- 3.1.2 select corresponding table columns -----------------
    # UI: select columns to
    # output$feedbacks <- renderUI
    observe({
      req(input$selected_mode == "gsea")
      req(rv$db_status == "selected")
      req(rv$file_upload_status == "uploaded")
      req(is.null(rv$infile_confirm) == T)
      req(!is.null(rv$rnk_or_deg))

      showModal(modalDialog(
        title = "Select corresponding columns to continue",

        fluidRow(
          column(12,
            wellPanel(
              # shiny::HTML("<p style='font-style:italic'>Select corresponding columns</p>"),
              materialSwitch(
                inputId = "mcol",
                label = HTML("<span style='font-size:110%'>Missing column names in your query file?</span>"),
                value = F, inline = TRUE, width = "100%",
                status = "danger"
              ),

              uiOutput("feedback_filecontent_deg"),
              uiOutput("feedback_filecontent_rnk"),
              # uiOutput("feedback_filecontent_confirm"),
              style = paste0("background:",bcol3)
            )
          ),
          column(6,
                 textInput("f_name",label = HTML(paste0("Name your query: ",add_help("name_q"))),value = rv$rnkll,width = "100%")
                 ,bsTooltip("name_q",HTML("The figures/results to be downloaded will be named according to the input here")
                           ,placement = "top")


          ),
          column(6,
                 uiOutput("ui_num")


          ),
          column(12, style="word-break:break-all;",
                 p("Review your uploaded file:"),
                 uiOutput("feedback_filecontent")
          )
          ,column(12,#align="right",
                  bsButton(
                    "filecontent_reset",
                    "Reset upload",
                    # block = TRUE,
                    style = "default"
                  )
          )
        ),

        easyClose = F,
        footer = fluidRow(
          column(12,
            bsButton(
              "filecontent_confirm",
              h4("Confirm and continue!"),
              block = TRUE,
              style = "primary"
            )
          )
        )
      ))

    })
    
    observeEvent(input$filecontent_reset,{
      reset_rnk()
      removeModal()
    })

    # ------------ 3.1.2.2 select numeric namespace -----------
    output$ui_num <- renderUI({
      req(input$gene_identifier == "other")
      req(input$selected_species != "" && input$selected_species != "other")

      r_num_acc()
    })

# ------------- 3.1.3 Upload and reset example RNK/DE --------------
    observeEvent(input$loadExampleRNK,{
        rv$example_file = NULL
        if(input$selected_species == ""){
          shinyalert("Please select your species of interest.")
        }else if(input$selected_species == "other"){
          shinyalert("Example data unavailable for custom GMT. Select a supported species for a trial run.")
        }else{
          reset_rnk()

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
            
            # saveRDS(rv$infile_name, file = "rvs/infile_name.rds")
            # saveRDS(rv$infile_path, file = "rvs/infile_path.rds")
        }
    })

    observeEvent(input$loadExampleDEG,{
        rv$example_file = NULL
        if(input$selected_species == ""){
          shinyalert("Please select your species of interest.")
        }else if(input$selected_species == "other"){
          shinyalert("Example data unavailable for custom GMT. Select a supported species for a trial run.")
        }else{
          reset_rnk()

          updateRadioButtons(
            session,
            "gene_identifier",
            selected = "symbol"
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


# ----------------- 3.1.4 check and store input file content into rv$data_head -----------------------
    observeEvent(rv$infile_name,{
        # req(is.null(rv$infile_name)==F)
        rv$infile_check=NULL
        rv$input_symbol = NULL
        rv$gene_lists_mat1 = NULL
        rv$run = NULL
        print(rv$infile_name)

        rv$rnkll <- strsplit(isolate(rv$infile_name),"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1] # add value to rv
        print(rv$rnkll)
        ranks <- read_delim(isolate(rv$infile_path), ",", locale = locale(encoding = 'ISO-8859-1'))# , escape_double = FALSE, trim_ws = TRUE)

        if(ncol(ranks)==1){
            ranks <- read_delim(isolate(rv$infile_path), "\t", locale = locale(encoding = 'ISO-8859-1'))# , escape_double = FALSE, trim_ws = TRUE)
        }

        for(i in seq_along(colnames(ranks))){
            #delete the unrecognized character
            colnames(ranks)[i] <- stringr::str_replace_all(colnames(ranks)[i],"[^(a-z0-9A-Z+><)|[:punct:]]", "")

            if(is.character(ranks[[i]])){
              ranks[[i]] <- stringr::str_replace_all(ranks[[i]],"[^(a-z0-9A-Z+><)|[:punct:]]", "")
            }
          }
          ranks =  ranks %>% #[complete.cases(ranks), ]
          dplyr::select_if(~sum(!is.na(.)) > 0)

        # detect if RNK or DEG
        if(ncol(ranks)==2){
            rv$rnk_or_deg = "rnk"
        }else if(ncol(ranks)>2){
            rv$rnk_or_deg = "deg"
        }else{
            shinyalert("You uploaded a file with < 2 columns. Please click the help button for accepted file formats.")
        }

        # save had data into RV
        rv$data_head = ranks
        rv$data_head_o = ranks
    })

    # ----------------- 3.1.5 Return RNK -----------------------
    observeEvent(input$filecontent_confirm,{
      # rename query
      if(input$f_name != ""){rv$rnkll = input$f_name}

      # read in file data
        data = rv$data_head_o
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
            removeModal()

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
            removeModal()

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

    # ----------------- 3.1.6 check if duplicated genes -----------------------

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
######      3.2.1 GList mode: input gene lists  ######
#====================================================#
    output$ui_glist <- renderUI({
        req(input$selected_mode == "glist")
        # req(input$selected_species != "")
        # req(is.null(rv$dbs)==F)
      if(input$selected_species == "other"){
        noo = p("2. Input your genes:",add_help("gene_list_q", style="font-size:medium;padding:3px 0 0 0;position:absolute;right:0.8em;"))
      }else{
        noo = p("3. Input your genes (",
    tags$style(type = "text/css", "#load_example_glist {display: inline-block;height: 20px;padding: 0;vertical-align: baseline;}"),
    add_help("gene_list_q", style="font-size:medium;padding:3px 0 0 0;position:absolute;right:0.8em;"),
    actionLink("load_example_glist", label = tags$u("example data")

    ),
    "):")
      }

      fluidRow(
        column(
          width = 12,
          bsTooltip("gene_list_q", "List of genes or proteins, newline-delimited", placement = "top"),
          div(
            id = "input_list_box",
              textAreaInput(
              inputId = "gene_list",
              label = noo,
              placeholder = "Paste your genes here ...",
              height = 110
            )
          )
        ),
        column(
          12,
          div(id = "ora_num_box",
              uiOutput("ora_num"))
        ),
        column(id="namelst_box",
          width = 12,
          fluidRow(
            column(6,
                   textInput(
                     "glist_name",
                     NULL,
                     placeholder = 'Name your list ...'
                   )
            ),
            column(6, align="right",
                   div(
                     style="display: inline-block;vertical-align:top;",
                     bsButton(
                       inputId = "gene_list_clear",
                       label = "Reset",
                       style = "default"
                     )
                   )
                   ,div(
                     style="display: inline-block;vertical-align:top;",
                     uiOutput("glist_add_button")
                     
                   )
            )
          )
          
          
        )

      )
    })

    # numeric identifier
    output$ora_num <- renderUI({
      req(input$gene_identifier == "other")
      req(input$selected_species != "other")
      r_num_acc()
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

    #---------------- 3.2.2 read in GList-----------------
    
    # observe if genes are uploaded
    observeEvent(rv$gene_lists,{
      if (is.null(rv$gene_lists)==F){
        shinyjs::disable("gene_list")
        shinyjs::disable("glist_name")
        shinyjs::disable("num_acc")
      }
      else if (is.null(rv$gene_lists)){
        shinyjs::reset("gene_list")
        shinyjs::enable("gene_list")
        shinyjs::reset("glist_name")
        shinyjs::enable("glist_name")
        shinyjs::reset("num_acc")
        shinyjs::enable("num_acc")
        
        updateTextAreaInput(session,
                            inputId = "gene_list",
                            value = ""
        )
      }
    })
    
    # from input field
    observeEvent(input$gene_list_add,{
        species = isolate(input$selected_species)
        # print(input$gene_list)
        if(nchar(species)>2){
            if(input$gene_list != ""){
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
                    if(input$gene_identifier == "other" && input$selected_species != "other"){
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
                          rv$gene_lists_mat2 = lst[[3]]

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
              shinyalert("Please input your query. Click example data for a trial run.")
            }
        }else{
          shinyalert("Please select species that matches your query.")
        }
        # }else{
        #     rv$gene_lists = rv$data_glist
        #
        #     # successfully submitted
        #     rv$glist_status = "submitted"
        # }
    })

    # -------------- 3.2.3 clear GList input ---------------------------
    observeEvent(input$gene_list_clear, {
        # rv$run = NULL

        rv$glist_check = NULL
        rv$gene_lists = NULL
        rv$gene_lists_after = NULL

        # updateTextInput(session,
        #                 inputId = "glist_name",
        #                 value = NULL)
    })

    #----------- 3.2.4 Example GList --------------
    observeEvent(input$load_example_glist,{
      print(gsub(";","\n",glist_example[input$selected_species][[1]]))
        if(input$selected_species == ""){
          shinyalert("Please select your species of interest.")
        }else if(input$selected_species == "other"){
          shinyalert("Example data unavailable for custom GMT. Select a supported species for a trial run.")
        }else{
            updateTextAreaInput(session,
                                inputId = "gene_list",
                                value = gsub(";","\n",glist_example[input$selected_species][[1]])
            )
        }
    })




#---------- 4. run parameters & confirm buttons ---------
    # UI confirm GSEA
    output$run_btn <- renderUI({
      req(rv$db_status == "selected")

      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
        aid = "confirm1"; alabel = "RUN GSEA!"
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
        aid = "confirm2"; alabel = "RUN ORA!"
      }


      div(
        actionBttn(aid,
                   alabel,
                   style=rv$run_btn_style, color=rv$run_btn_color, size = "lg",
                   icon = icon("play-circle"),
                   block = TRUE)
        ,
        div(
          style="position: absolute; right: 0.8em; top: -0.4em;",
          uiOutput("ui_gsea_par")

        )
      )

    })

    # --------------- 4a. UI GSEA parameter ---------------
    output$ui_gsea_par <- renderUI({
      if(input$selected_mode == "gsea"){
        req(is.null(rv$rnk_check)==F)
        req(is.null(rv$rnkgg)==F)
      }else if(input$selected_mode == "glist"){
        req(is.null(rv$glist_check)==F)
        req(is.null(rv$gene_lists_after)==F)
      }

      dropdownButton(
        width = "300px",circle = TRUE, status = "info",
        size = "xs",
        icon = icon("gear"),# class = "opt"),
        up = T,
        tooltip = tooltipOptions(title = "Click to adjust run parameters"),

        fluidRow(
          br(),
          column(12,
          #   width = 12, title = "Advanced run parameters", status = "warning", collapsible = T, collapsed = T,
            wellPanel(
              h4("Advanced run parameters"),
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
              ,materialSwitch(
                inputId = "q_dynamic",
                label = HTML(paste0("Allow dynamic adjustment on adjusted P-value threshold",add_help("q_dynamic_q"))),
                value = rv$q_dynamic, inline = F, width = "100%",
                status = "danger"
              )
              ,style = "background:#e6f4fc;"
            ),
            bsTooltip("mymin_q", "Minimum gene set size", placement = "top"),
            bsTooltip("mymax_q", "Maximum gene set size", placement = "top")
          ,bsTooltip("q_dynamic_q","If TRUE (the default), easyGSEA will dynamically adjust the adjusted P-value threshold to capture the most significantly enriched while minimizing false positives. Switch to FALSE if you have multiple datasets to be consistent in the threshold."
                     ,placement = "top")
          )
        )
      )


    })
    
    # number of permutation for GSEA run
    output$ui_nperm <- renderUI({
      req(input$selected_mode == "gsea")
      div(
        numericInput("nperm",
                     HTML(paste0("# perm:",add_help("nperm_q")))
                     ,rv$gperm),
        bsTooltip("nperm_q", "No. of permutations", placement = "top")
      )
    })
    
    # dynamically change padj
    observeEvent(input$q_dynamic,{
      rv$q_dynamic <- input$q_dynamic
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

        species <- isolate(input$selected_species)

        # save dbs for plots
        if(species != "other"){
          rv$bar_pathway <- rv$dbs
          rv$bubble_pathway <- rv$dbs
          rv$volcano_pathway <- rv$dbs
          rv$vis_pathway <- rv$dbs
        }else{
          rv$gmt_cs = lapply(rv$gmt_cs,function(x) {strsplit(x,"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1]})

          rv$bar_pathway <- rv$gmt_cs
          rv$bubble_pathway <- rv$gmt_cs
          rv$volcano_pathway <- rv$gmt_cs
          rv$vis_pathway <- rv$gmt_cs
        }


        withProgress(message = "Running GSEA analysis...",value = 0.2, {

            # ------ read GMTs & run fgsea ------ #
            # initialize
            errors = 0

            if(species != "other"){
              for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(rv$db_modal)==F){
                  inputs = input[[db_id]]
                }else{
                  inputs = gmt_collections_selected[[species]][[collection]]
                }

                for(cat_name in inputs){
                  gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]

                  run_gsea(cat_name, gmt_path, ranks,errors)
                }
              }
            }else{
              for(i in seq_along(rv$gmt_cs)){
                gmt_path = rv$gmt_cs_paths[[i]]
                run_gsea(rv$gmt_cs[[i]], gmt_path, ranks,errors)
              }
            }


            if(errors > 0 && nrow(rv$fgseagg)<1){
              if(species != "other"){
                db_selected = names(rv$dbs)
                db_selected = paste(db_selected,collapse = "; ")
                # ErrorMessage <- conditionMessage(attr(frun, "condition"))  # the error message
                #show a modal dialog if there is an error reading files causing crash
                showModal(modalDialog(
                  title = h3(HTML("Please click the gear button and adjust <b>Advanced run parameters</b>")),
                  tags$li(h4(paste0("Database(s): ",db_selected))),
                  tags$li(h4(paste0("No gene sets available after filtering by min=",rv$gmin
                                    ," and max=",rv$gmax))),

                  size = "l",
                  easyClose = TRUE
                ))
              }else{
                db_selected = paste(rv$gmt_cs,collapse = "; ")

                showModal(modalDialog(
                  title = h3(HTML("Analysis failed")),
                  h4(HTML("Please check if the uploaded GMTs are in correct format. If yes, click the gear button and adjust <b>Advanced run parameters</b>")),
                  tags$li(h4(paste0("Database(s): ",db_selected))),
                  tags$li(h4(paste0("No gene sets available after filtering by min=",rv$gmin
                                    ," and max=",rv$gmax))),

                  size = "l",
                  easyClose = TRUE
                ))
              }

            }else{
              # count number of filtered GSs in GMTs
              l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
              rv$gmts_length = sum(l)

              # determine if success or warnings
              if(nrow(rv$fgseagg)>0){
                rv$run = "success"
                rv$run_n = rv$run_n + 1
              } else {
                rv$run = "failed"
              }
              incProgress(0.1)
            }
        })
        # saveRDS(rv$fgseagg, file = "rvs/fgseagg.rds")
        # saveRDS(rv$gmts, file = "rvs/gmts.rds")
        # saveRDS(rv$gmts_length, file = "rvs/gmts_length.rds")
        # saveRDS(rv$gmt_cs_paths, file = "rvs/gmt_cs_paths.rds")
        # saveRDS(rv$db_modal, file = "rvs/db_modal.rds")
        # saveRDS(rv$gmt_cs, file = "rvs/gmt_cs.rds")
        # saveRDS(rv$sd_high, file = "rvs/sd_high.rds")
        # saveRDS(rv$gmin, file = "rvs/gmin.rds")
        # saveRDS(rv$gmax, file = "rvs/gmax.rds")
        # saveRDS(rv$gperm, file = "rvs/gperm.rds")
        # saveRDS(rv$bar_pathway, file = "rvs/bar_pathway.rds")
        # saveRDS(rv$bubble_pathway, file = "rvs/bubble_pathway.rds")
        # saveRDS(rv$run_n, file = "rvs/run_n.rds")

    })

    #===============================================#
    #####             4.2 run GList!!!          #####
    #===============================================#

    observeEvent(input$confirm2, {
        rv$run_mode = "glist"
        species <- isolate(input$selected_species)

        # reset RVs
        reset_rvs()

        # read in parameters

        genelist = toupper(rv$gene_lists_after)

        if(is.null(input$mymin)==F){rv$gmin=input$mymin}
        if(is.null(input$mymax)==F){rv$gmax=input$mymax}

        # save dbs for plots
        if(species != "other"){
          rv$bar_pathway = rv$dbs
          rv$bubble_pathway = rv$dbs
          rv$vis_pathway <- rv$dbs
          
        }else{
          rv$gmt_cs = lapply(rv$gmt_cs,function(x) {strsplit(x,"\\.(?=[^\\.]+$)", perl=TRUE)[[1]][1]})

          rv$bar_pathway = rv$gmt_cs
          rv$bubble_pathway = rv$gmt_cs
          rv$vis_pathway <- rv$gmt_cs
          
        }


        withProgress(message = "Running ORA analysis...",value = 0.2, {

            # ------ read GMTs & run fgsea ------ #
            # initialize
            errors = 0

            if(species != "other"){
              for(collection in sort(names(gmt_collections_paths[[species]]))){
                db_id = paste0(species,gsub(" ","_",collection))
                if(is.null(rv$db_modal)==F){
                  inputs = input[[db_id]]
                }else{
                  inputs = gmt_collections_selected[[species]][[collection]]
                }

                for(cat_name in inputs){
                  gmt_path = gmt_collections_paths[[species]][[collection]][[cat_name]]
                  run_ora(cat_name,gmt_path,genelist,errors)
                }

              }
            }else{
              for(i in seq_along(rv$gmt_cs)){
                gmt_path = rv$gmt_cs_paths[[i]]
                run_ora(rv$gmt_cs[[i]], gmt_path, genelist,errors)
              }
            }


            if(errors > 0 && is.null(rv$fgseagg)){
              db_selected = names(rv$dbs)
              db_selected = paste(db_selected,collapse = "; ")
              # ErrorMessage <- conditionMessage(attr(frun, "condition"))  # the error message
              #show a modal dialog if there is an error reading files causing crash
              showModal(modalDialog(
                title = h3(HTML("Please click the gear button to adjust <b>run parameters</b>")),
                tags$li(h4(paste0("Database(s): ",db_selected))),
                tags$li(h4(paste0("No gene sets available after filtering by min=",rv$gmin
                                  ," and max=",rv$gmax))),

                size = "l",
                easyClose = TRUE
                ,footer = modalButton("OK")
              ))
            }else{
              # count number of filtered GSs in GMTs
              l = unlist(lapply(rv$gmts, function(x){return(length(x)>=rv$gmin && length(x)<=rv$gmax)}))
              rv$gmts_length = sum(l)

              # determine if success or warnings
              if(is.null(rv$fgseagg)==F && nrow(rv$fgseagg)>0){
                rv$run = "success"
                rv$run_n = rv$run_n + 1

              } else {
                rv$run = "failed"
              }
              incProgress(0.1)
            }


        })
    })

    # ---------------------- 4.3 render Modal upon successful run ----------------
    observeEvent(rv$run_n,{
      showModal(modalDialog(
        easyClose = F,
        fluidRow(
          column(
            12,
            h2("Analysis complete!")
            ,br(),column(12,uiOutput("run_summary_gsea"))
            ,br(),br(),br(),guide_box("msg1")

          )
        )
        ,footer = NULL
      ))
    },ignoreInit = T)

    # ------------ clike button to Enrichment Results tab ------------
    observeEvent(input$msg1,{
      removeModal()
      updateTabItems(session, "tabs", "kegg")
    })
    
    # ------------ demo's nav to next tab UI ----------------
    output$demo_nav <- renderUI({
      req(rv$demo_mode != "")
      if(input$selected_mode == "gsea"){
        req(input$confirm1 == 0)
        
      }else{
        req(input$confirm2 == 0)
        
      }
      
      column(12,
        guide_box("msg2",size="sm")
        
      )
    })
    
    observeEvent(input$msg2,{
      removeModal()
      updateTabItems(session, "tabs", "kegg")
    })
