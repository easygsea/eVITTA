# ----------- feedbacks about inputs -------------
# feedback run mode
output$feedback_runmode <- renderUI({
    HTML(
        "Mode of analysis:</br><b>",
        species_translate(input$selected_mode,run_modes),
        "</b><br/><br/>"
    )
})

# feedback species
output$feedback_species <- renderUI({
    # req(nchar(input$selected_species)>0)
    req(rv$db_status == "selected")
    HTML(
        "Species of interest:&nbsp<br/><b><i>",
        species_translate(input$selected_species),
        "</i></b><br/><br/>"
    )
    
})

# feedback databases
output$feedback_dbs <- renderUI({
    req(rv$db_status == "selected")
    db_selected = names(rv$dbs)
    HTML(
        "Selected databases:<br/><b>",
        paste(db_selected,collapse = "; "),
        "</b>"
    )
})

#==========================================#
#####      FEEDBACKS on GSEA run       #####
#==========================================#
# feedback file name
output$feedback_filename <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(is.null(rv$infile_name) == F)
    
    if(is.null(rv$example_file) == F){pref = " (our example data)"}else{pref = ""}
    
    HTML(
        "Query file",pref,":<br/><b>",
        rv$infile_name,
        "</b><br/>"
    )
})

# --------------- feedback on input file content --------------
output$feedback_filecontent <- renderTable({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(rv$file_upload_status == "uploaded")
    req(is.null(rv$infile_confirm) == T)
    
    df = rv$data_head_o %>%
        head(.,n=2) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x,2)) %>%
        dplyr::mutate_if(is.character, function(x) substr(x,1,7))
    
    arow = rep("...",ncol(df))
    
    rbind(df,arow)
        
})

# ----------- *** If no colnames, reset rv$data_head_o ----------
observeEvent(input$mcol,{
    if(input$mcol){
        rv$data_head_o = reset_colnames(rv$data_head)
    }else{
        rv$data_head_o = rv$data_head
    }
    session$sendCustomMessage(type = "resetValue", message = "gene_column")
    session$sendCustomMessage(type = "resetValue", message = "rank_column")
    session$sendCustomMessage(type = "resetValue", message = "logfc_column")
    session$sendCustomMessage(type = "resetValue", message = "p_column")
})

# colnames for DEG
output$feedback_filecontent_deg <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(rv$rnk_or_deg == "deg")
    req(rv$file_upload_status == "uploaded")
    req(is.null(rv$infile_confirm) == T)
    
    fluidRow(
        column(
            width = 4,
            radioButtons(
                inputId = "gene_column",
                label = HTML(paste0("Gene column:",
                                    add_help("gene_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_gene_names,colnames(rv$data_head_o))
            ),
            bsTooltip("gene_colq", "The genes", placement = "top"),
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "logfc_column",
                label = HTML(paste0("logFC column:",
                                   add_help("fc_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_fc_names,colnames(rv$data_head_o))
            ),
            bsTooltip("fc_colq", "Log-transformed fold changes of genes", placement = "top"),
            
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "p_column",
                label = HTML(paste0("P value column:",
                                    add_help("p_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_p_names,colnames(rv$data_head_o))
            ),
            bsTooltip("p_colq", "P values of differential expressions", placement = "top"),
            
        )
    )
})

# colnames for RNK
output$feedback_filecontent_rnk <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(rv$rnk_or_deg == "rnk")
    req(rv$file_upload_status == "uploaded")
    req(is.null(rv$infile_confirm) == T)
    
    fluidRow(
        column(
            width = 6,
            radioButtons(
                inputId = "gene_column",
                label = HTML(paste0("Gene column:",
                                    add_help("gene_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_gene_names,colnames(rv$data_head_o))
            ),
            bsTooltip("gene_colq", "The genes", placement = "top"),
        ),
        column(
            width = 6,
            radioButtons(
                inputId = "rank_column",
                label = HTML(paste0("Rank column:",
                                    add_help("rank_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_rank_names,colnames(rv$data_head_o))
            ),
            bsTooltip("rank_colq", "Rank scores of genes", placement = "top"),
            
        )
    )
})

# confirm file content
output$feedback_filecontent_confirm <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(rv$file_upload_status == "uploaded")
    req(is.null(rv$infile_confirm) == T)
    
    fluidRow(
        column(
            12,
            bsButton(
                "filecontent_confirm",
                "Confirm and continue!",
                block = TRUE,
                style = "primary"
            )
        )
    )
    
})

# --------------- feedback on converted RNK --------------
output$feedback_converted_rnk <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(is.null(rv$infile_check) == F)
    species = isolate(input$selected_species)
    if(rv$infile_check == "unmatch"){
        box(
            background = "red", width=12,
            "Please check if your selected species matches your query."
        )
    }else if(rv$infile_check == "wrong_rnk"){
        box(
            background = "red", width=12,
            "You probably uploaded a wrong RNK file. RNK should have 1st column as gene names and 2nd column as ranks (numeric). Please click on the help button to learn more and load our example RNK file for a try."
        )
    }else if(rv$infile_check == "wrong_deg"){
        box(
            background = "red", width=12,
            "You probably uploaded a wrong DEG file. DEG files should have at least a column with gene names, a column with logFC (numeric), and a column with P/FDR values (numeric). Please click on the help button to learn more and load our example DEG file for a try."
        )
    }else if(rv$infile_check == "pass"){
        if(rv$rnk_check == "none"){
            fluidRow(
                box(
                    background = "red", width=12,
                    "No ID detected in ",species_translate(species),"'s database. Please check if your query file is correct and/or if you have selected the right gene identifier and/or if your selected species matches your query."
                )
            )
        }else if(rv$rnk_check == "low"){
            fluidRow(
                column(
                    width = 12,
                    "RNK for pre-ranked GSEA run:"
                    ,uiOutput("ui_rnk_download")
                    ,dataTableOutput("converted_rnk")
                    ,paste0("No. of genes: ",
                           rv$total_genes_after, " / ",rv$total_genes)
                    
                ),
                box(
                    background = "red", width=12,
                    "Fewer than 50% of your query genes detected in ",species_translate(species),"'s database. Please check if your selected species matches your query.",
                )
            )
        }else if(rv$rnk_check == "pass"){
            fluidRow(
                column(
                    width = 12,
                    "RNK for pre-ranked GSEA run:"
                    ,uiOutput("ui_rnk_download")
                    ,dataTableOutput("converted_rnk")
                    ,paste0("No. of genes: ",
                           rv$total_genes_after, " / ",rv$total_genes)
                )
            )
        }
    }
})

# RNK table
output$converted_rnk <- DT::renderDataTable({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    df = data.frame(GeneName=names(rv$rnkgg),Rank=rv$rnkgg,stringsAsFactors = F) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x, digits=3))
        # dplyr::top_n(2) %>%
        # dplyr::mutate_if(is.numeric, function(x) as.character(x))
    
    # if(nrow(df)>1){
    #     df = df %>%
    #         dplyr::add_row(GeneName="...",Rank="...")
    # }
    
    df
}, options=list(scrollX=T, pageLength = 5, dom = 'tpr', pagingType = "simple"), rownames= FALSE)

#==========================================#
#####      FEEDBACKS on ORA run        #####
#==========================================#
# -------------- feedback on genes ------------
output$feedback_glist <- renderUI({
    req(input$selected_mode == "glist")
    req(rv$db_status == "selected")
    req(is.null(rv$glist_check) == F)

    HTML(
        "Your list: <b>",
        rv$rnkll,
        "</b><br/>",
        abbreviate_vector(rv$gene_lists),
        " (n=<b>",
        length(rv$gene_lists),
        "</b>)<br/><br/>"
    )
})

#-------------feedbacks on converted genes------------
output$feedback_converted_glist <- renderUI({
    req(input$selected_mode == "glist")
    req(rv$db_status == "selected")
    req(is.null(rv$glist_check) == F)
    species = isolate(input$selected_species)
    
    if(rv$glist_check == "none"){
        fluidRow(
            box(
                background = "red", width=12,
                "No ID detected in ",species_translate(species),"'s database. Please check if your gene list is correct and/or if your selected species matches your query."
            )
        )
    }else if(rv$glist_check == "low"){
        fluidRow(
            column(
                width = 12,
                HTML(
                    "Converted list: <b>",
                    rv$rnkll,
                    "</b><br/>",
                    abbreviate_vector(rv$gene_lists_after),
                    " (n=<b>",
                    length(rv$gene_lists_after),
                    "</b>)<br/><br/>"
                )
            ),
            box(
                background = "red", width=12,
                "Fewer than 50% of genes detected in ",species_translate(species),"'s database. Please check if your selected species matches your query.",
            ),
            br(),br()
        )
    }else if(rv$glist_check == "pass"){
        HTML(
            "Converted list: <b>",
            rv$rnkll,
            "</b><br/>",
            abbreviate_vector(rv$gene_lists_after),
            " (n=<b>",
            length(rv$gene_lists_after),
            "</b>)<br/><br/>"
       )
    }
})

# ---------------- feedback on GSEA/ORA results ---------------------
output$run_summary_gsea <- renderUI({
    req(is.null(rv$run)==F)
    req(rv$db_status == "selected")
    db_selected = names(rv$dbs)
    db_selected = paste(db_selected,collapse = "; ")
    
    if(rv$run == "success"){
        if(rv$run_mode == "gsea"){
            fluidRow(
                wellPanel(
                    style = paste0("background:",bcol2),
                    h4("Summary Report"),
                    # p(paste0("Mode of analysis: ",names(run_modes[run_modes == rv$run_mode]))),
                    tags$ul(
                        # tags$li(HTML("Species: <b><i>",species_translate(input$selected_species),"</b></i>")),
                        # tags$li(HTML("Databases: <b>",db_selected,"</b>")),
                        # tags$li(HTML("Query name:<b>",rv$rnkll,"<\b>")),
                        tags$li(HTML("Gene set size filters min=",rv$gmin," max=",rv$gmax," results in ",rv$gmts_length," / ",length(rv$gmts)," gene sets")),
                        # tags$li(HTML("Number of permutation=",rv$gperm)),
                        tags$li(HTML("<b>",rv$no_down_05,"</b> (down) <b>",rv$no_up_05,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_down_01,"</b> (down) <b>",rv$no_up_01,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.25"))
                    )
                )
            )
        }else if(rv$run_mode == "glist"){
            fluidRow(
                wellPanel(
                    style = paste0("background:",bcol2),
                    h4("Summary Report"),
                    # p(paste0("Mode of analysis: ",names(run_modes[run_modes == rv$run_mode]))),
                    tags$ul(
                        # tags$li(HTML("Species: <b><i>",species_translate(input$selected_species),"</b></i>")),
                        # tags$li(HTML("Databases: <b>",db_selected,"</b>")),
                        # tags$li(HTML("Query name:<b>",rv$rnkll,"<\b>")),
                        tags$li(HTML("Gene set size filters min=",rv$gmin," max=",rv$gmax," results in ",rv$gmts_length," / ",length(rv$gmts)," gene sets")),
                        tags$li(HTML("<b>",rv$no_up_05,"</b> gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_up_01,"</b> gene sets are significantly enriched at P.adj < 0.25"))
                    )
                )
            )
        }
    }else if(rv$run == "failed"){
        fluidRow(
            box(
                background = "red", width = 12,
                HTML("No enrichment results for <b>",rv$rnkll,"</b>. Please check if species matches your query and/or if you have selected the right gene identifier & its column and/or if your input file/gene list is correct.")
            )
        )
    }
})

# UI ratioGroupButton ----------------------------------------
    # output$body_1_ui <- renderUI({
        # if(is.null(rv$run)==T){
        #     box(title="Notification", status="warning",
        #         "Results will be shown in this panel."
        #     )
        # }
        # else if(rv$run == "success"){
    output$radio_buttons <- renderUI({
        # req(rv$run == "success")
        radioGroupButtons(
            inputId = "summary_type",
            choiceNames = list("Summary", span(icon("align-left"),"ID conversion")), #"Bar plot","Bubble plot","Volcano plot",
            choiceValues = list("summary", "id"), #"bar", "bubble","volcano",
            selected = "summary",
            # checkIcon = list(
            #     yes = icon("check-square"),
            #     no = icon("square-o")
            # ),
            status = "primary",
            direction = "horizontal"
        )
    })

# ------------ UI summary text ---------------
output$summary_txt <- renderUI({
    fluidRow(
        column(
            4,
            uiOutput("feedback_runmode")
        ),
        column(
            4,
            uiOutput("feedback_species")
        ),
        column(
            4,
            uiOutput("feedback_filename")
        ),
        column(
            12,
            uiOutput("feedback_dbs")
        )
    )
})
# ------------ UI summary box ----------------
    output$summary_box <- renderUI({
        fluidRow(
            column(
                width = 12,
                
                # uiOutput("feedback_filecontent"),
                # uiOutput("feedback_filecontent_deg"),
                # uiOutput("feedback_filecontent_rnk"),
                # uiOutput("feedback_filecontent_confirm"),
                
                uiOutput("feedback_converted_rnk"),
                uiOutput("feedback_glist"),
                uiOutput("feedback_converted_glist")
                
            )
            # ,column(
            #     width = 6, offset = 1,
            #     uiOutput("run_summary_gsea")
            # )
        )
        
    })

# --------- UI ID conversion ----------
output$id_box <- renderUI({
    req(is.null(rv$gene_lists_mat)==F)
    box(
        title = span(icon("table"),"ID conversion"), width = 12, status = "primary", #span(img(src = "easygsea_bw.tiff", height = 40))
        
        # uiOutput("id_none"),
        # div(
        #     style = "display: inline-block;vertical-align:top;",
        #     
        # )
        uiOutput("ui_mat_download")
        
        ,br()
        ,dataTableOutput("id_conversion_table")
    )
})

# # UI ID conversion table
# # box to display if no ID conversion
# output$id_none <- renderUI({
#     req(is.null(rv$gene_lists_mat))
#     
#     wellPanel(
#         "ID conversion table available when applicable."
#     )
# })

# render ID conversion table
output$id_conversion_table <- DT::renderDataTable({
    rv$gene_lists_mat
}, plugins="ellipsis", options=list(scrollX=T, pageLength = 5,  pagingType = "simple",
                                    columnDefs = list(list(
                                        targets = 5,
                                        render = JS(
                                            "function(data, type, row, meta) {",
                                            "return type === 'display' && data.length > 18 ?",
                                            "'<span title=\"' + data + '\">' + data.substr(0, 18) + '...</span>' : data;",
                                            "}")))), rownames= FALSE)

# download ID conversion button
output$ui_mat_download <- renderUI({
    req(is.null(rv$gene_lists_mat) == F)
    downloadBttn("mat_download",
                   label = "Download ID conversion table (.csv)", style = rv$dbtn_style,
                   color = rv$dbtn_color, size=rv$dbtn_size, block = TRUE
    )
})

# download ID conversion table
output$mat_download <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_id_conversion.csv")},
    content = function(file) {
        df = rv$gene_lists_mat
        fwrite(df, file, sep=",", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=T)
    })

# UI download RNK --------------
output$ui_rnk_download <- renderUI({
    req(is.null(rv$rnkgg) == F)
    req(input$selected_mode == "gsea")
    
    downloadBttn("rnk_download",
                   label = "Download RNK (.rnk)", style = rv$dbtn_style,
                 color = rv$dbtn_color, size=rv$dbtn_size, block = TRUE
    )
})

# download RNK table
output$rnk_download <- downloadHandler(
    filename = function() {paste0(rv$rnkll,".rnk")},
    content = function(file) {
        df = data.frame(GeneName=names(rv$rnkgg),Rank=rv$rnkgg,stringsAsFactors = F)
        fwrite(df, file, sep="\t", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=F)
    })