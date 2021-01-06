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
    if(input$selected_species != "other"){
        db_selected = names(rv$dbs)
    }else{
        db_selected = rv$gmt_cs
    }
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
        "Query file",pref,":<br/><div style = 'word-wrap: break-word;'><b>",
        rv$infile_name,
        "</b></div><br/>"
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
                label = HTML(paste0("Gene column: ",
                                    add_help("gene_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_gene_names,colnames(rv$data_head_o))
            ),
            bsTooltip("gene_colq", "The genes, proteins or probes (e.g, Gene, Symbol, Ensembl, Affymetrix ID)", placement = "top"),
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "logfc_column",
                label = HTML(paste0("logFC column: ",
                                   add_help("fc_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_fc_names,colnames(rv$data_head_o))
            ),
            bsTooltip("fc_colq", "The log-transformed fold changes of genes (e.g. logFC), or the direction of fold changes (e.g. -1, +1)", placement = "top"),
            
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "p_column",
                label = HTML(paste0("P value column: ",
                                    add_help("p_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_p_names,colnames(rv$data_head_o))
            ),
            bsTooltip("p_colq", "The P values of differential expressions (e.g. P, P-value)", placement = "top"),
            
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
                label = HTML(paste0("Gene column: ",
                                    add_help("gene_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_gene_names,colnames(rv$data_head_o))
            ),
            bsTooltip("gene_colq", "The genes, proteins or probes (e.g, Gene, Symbol, Ensembl, Affymetrix ID)", placement = "top"),
        ),
        column(
            width = 6,
            radioButtons(
                inputId = "rank_column",
                label = HTML(paste0("Rank column: ",
                                    add_help("rank_colq"))),
                choices = colnames(rv$data_head_o),
                selected = match_colnames(col_rank_names,colnames(rv$data_head_o))
            ),
            bsTooltip("rank_colq", "The rank scores (e.g. Rank, Rank score)", placement = "top"),
            
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
                    HTML("No ID detected in <i><b>",species_translate(species),"</b></i>'s database. Please check if"),
                    tags$ul(
                        tags$li("The selected species matches your query, and/or")
                        ,tags$li("You have selected the right gene identifier, and/or")
                        ,tags$li("The uploaded file is correct")
                    )
                )
            )
        }else if(rv$rnk_check == "low"){
            fluidRow(
                column(
                    width = 12,
                    div(
                        style = "display: inline-block;vertical-align:baseline;",
                        "Converted RNK for pre-ranked GSEA run:  "
                    )
                    ,div(
                        style = "display: inline-block;vertical-align:baseline;",
                        uiOutput("ui_rnk_download")
                    )
                    
                ),
                column(
                    12,
                    tableOutput("converted_rnk")
                ),
                column(
                    12, align = "right",
                    paste0("Total number of genes: ", rv$total_genes_after, " / ",rv$total_genes)
                ),
                box(
                    background = "red", width=12,
                    HTML("Fewer than 50% of your query genes detected in <i><b>",species_translate(species),"</b></i>'s database. Please check if your selected species matches your query."),
                )
            )
        }else if(rv$rnk_check == "pass"){
            fluidRow(
                column(
                    width = 12,
                    div(
                        style = "display: inline-block;vertical-align:baseline;",
                        "Converted RNK for pre-ranked GSEA run:  "
                    )
                    ,div(
                        style = "display: inline-block;vertical-align:baseline;",
                        uiOutput("ui_rnk_download")
                    )
                    
                ),
                column(
                    12,
                    tableOutput("converted_rnk")
                ),
                column(
                    12, align = "right",
                    paste0("Total number of genes: ", rv$total_genes_after, " / ",rv$total_genes)
                )
            )
        }
    }
})

# ----------- RNK table --------------
output$converted_rnk <- renderTable({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    df = data.frame(GeneName=names(rv$rnkgg),Rank=rv$rnkgg,stringsAsFactors = F) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x, digits=3)) %>%
        dplyr::mutate_if(is.numeric, function(x) as.character(x))
    
    if(nrow(df)>2){
        df = df %>%
            dplyr::top_n(2) %>%
            dplyr::add_row(GeneName="... ...",Rank="... ...")
    }
    
    df
},width = "100%") #, options=list(scrollX=T, pageLength = 5, dom = 'tpr', pagingType = "simple"), rownames= FALSE

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
                HTML("No ID detected in <i><b>",species_translate(species),"</b></i>'s database. Please check if"),
                tags$ul(
                    tags$li("The selected species matches your query, and/or")
                    ,tags$li("Your input gene list is correct")
                    
                    )
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
                HTML("Fewer than 50% of genes detected in <i><b>",species_translate(species),"</b></i>'s database. Please check if your selected species matches your query."),
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
                        tags$li(HTML("<b>",rv$no_down_25,"</b> (down) <b>",rv$no_up_25,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.25")),
                        tags$li(HTML("<b>",rv$no_down_05,"</b> (down) <b>",rv$no_up_05,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_down_01,"</b> (down) <b>",rv$no_up_01,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.01"))
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
                        tags$li(HTML("<b>",rv$no_up_25,"</b> gene sets are significantly enriched at P.adj < 0.25")),
                        tags$li(HTML("<b>",rv$no_up_05,"</b> gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_up_01,"</b> gene sets are significantly enriched at P.adj < 0.01"))
                    )
                )
            )
        }
    }
})

# --------- error if no genes found in databases ---------------
output$run_error <- renderUI({
    req(rv$run == "failed")
    
    if(input$selected_species != "other"){
        msg = div(
            HTML("No enrichment results for <b>",rv$rnkll,"</b>. Please check if ")
            ,tags$ul(
                tags$li("The selected species matches your query, and/or")
                ,tags$li("You have selected the right gene identifier as well as its column, and/or")
                ,tags$li("Your input file/gene list is correct.")
                ,tags$li("Adjust the maximum and minimum gene set sizes according to the gene set library(ies) you've chosen.")
            )
        )
    }else{
        msg = div(
            HTML("No enrichment results for <b>",rv$rnkll,"</b>. Please check if ")
            ,tags$ul(
                tags$li("Your uploaded GMT(s) is (are) correct, and/or")
                ,tags$li("Your uploaded GMT(s) match(es) the species in your query, and/or")
                ,tags$li("The gene identifier of your query matches your uploaded GMT(s), and/or")
                ,tags$li("Your input file/gene list is correct.")
                ,tags$li("Adjust the maximum and minimum gene set sizes according to the gene set library(ies) you've chosen.")
            )
        )
    }
    
    fluidRow(
        box(
            background = "red", width = 12,
            msg
        )
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
                ,uiOutput("run_error")
                
            )
            # ,column(
            #     width = 6, offset = 1,
            #     uiOutput("run_summary_gsea")
            # )
        )
        
    })

# --------- UI ID conversion ----------
output$id_box <- renderUI({
    if(input$selected_mode == "gsea"){
        req(is.null(rv$gene_lists_mat1)==F)
    }else if(input$selected_mode == "glist"){
        req(is.null(rv$gene_lists_mat2)==F)
    }
    
    box(
        title = span(icon("align-left"),"Gene ID conversion"), width = 12, status = "primary", #span(img(src = "easygsea_bw.tiff", height = 40))
        
        div(
            id = "DataTables_Table_0_wrapper",
            fluidRow(
                
                column(
                    12,
                    div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput("ui_mat_download")
                    )
                    ,div(
                        style = "display: inline-block;vertical-align:top;",
                        uiOutput("ui_deg_download")
                    )
                )
            )
            ,br()
            ,fluidRow(
                column(
                    12
                    ,dataTableOutput("id_conversion_table")
                )
            )
        )
        
    )
})

# render ID conversion table
output$id_conversion_table <- DT::renderDataTable({
    
    if(input$selected_mode == "gsea"){
        df = rv$gene_lists_mat1
    }else if(input$selected_mode == "glist"){
        df = rv$gene_lists_mat2
    }
    
    df_no(df,scrollY = "148px")
#     df
#     
# }, plugins="ellipsis", options = dt_options()
# options=list(scrollX=T, pageLength = 5,  pagingType = "simple",
#                                     columnDefs = list(list(
#                                         targets = 5,
#                                         render = JS(
#                                             "function(data, type, row, meta) {",
#                                             "return type === 'display' && data.length > 18 ?",
#                                             "'<span title=\"' + data + '\">' + data.substr(0, 18) + '...</span>' : data;",
#                                             "}")))), rownames= FALSE
})

# download ID conversion button
output$ui_mat_download <- renderUI({
    downloadBttn("mat_download",
                   label = "Download ID conversions (.csv)", style = rv$dbtn_style,
                   color = rv$dbtn_color, size=rv$dbtn_size, block = F
    )
})

# download ID conversion table
output$mat_download <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_id_conversion.csv")},
    content = function(file) {
        if(input$selected_mode == "gsea"){
            df = rv$gene_lists_mat1
        }else if(input$selected_mode == "glist"){
            df = rv$gene_lists_mat2
        }
        fwrite(df, file, sep=",", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=T)
    })

# ----------- converted DEG download ---------
output$ui_deg_download <- renderUI({
    req(rv$rnk_or_deg == "deg")
    req(input$selected_mode == "gsea")
    
    div(id="deg_download_btn",
        downloadBttn("deg_download",
                     label = "Download converted DEG table (.csv)", style = rv$dbtn_style,
                     color = rv$dbtn_color, size=rv$dbtn_size, block = F
        ),
        bsTooltip("deg_download_btn",HTML("Download converted DEG table and proceed to <b>easyVizR</b> for multiple comparisons")
                  ,placement = "top")
    )
    
})

# download converted DEG table
output$deg_download <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_convertedDEG.csv")},
    content = function(file) {
        
        fwrite(rv$data_head_o, file, sep=",", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=T)
    })

# UI download RNK --------------
output$ui_rnk_download <- renderUI({
    req(is.null(rv$rnkgg) == F)
    req(input$selected_mode == "gsea")
    
    downloadBttn("rnk_download",
                   label = "Download RNK (.rnk)", style = rv$dbtn_style,
                 color = rv$dbtn_color, size=rv$dbtn_size, block = F
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

# UI manage GMTs ----------------
output$gmt_box <- renderUI({
    req(input$selected_species == "other")
    req(is.null(rv$db_status)==T || rv$db_status == "modify")

    fluidRow(column(12,
                    box(width=12,
                        title = span(icon("tasks"), "Review and manage uploaded GMTs (maximum 300MB)"), status = "primary", solidHeader = F,
                        tabPanel("Loaded files", 
                                 uiOutput("delete_gmt"),
                                 uiOutput("delete_gmt_confirm"),
                        )
                    ),
                    
    ))
})

output$delete_gmt <- renderUI({
    if(length(rv$gmt_cs) >= 1){
        multiInput(inputId = "delete_gmt",
                   label = NULL,
                   choices = rv$gmt_cs,
                   width = "100%",
                   options = list(
                       enable_search = FALSE,
                       non_selected_header = "Loaded GMT(s):",
                       selected_header = "Delete GMT(s):")
        )
    } else {
        HTML("No GMT loaded.")
    }
})

output$delete_gmt_confirm <- renderUI({
    req(length(rv$gmt_cs) >= 1)
    
    bsButton("delete_gmt_confirm", "Confirm and delete", style = "default")
})

observeEvent(input$delete_gmt_confirm,{
    to_delete_i <- which(rv$gmt_cs %in% input$delete_gmt)
    
    rv$gmt_cs <- rv$gmt_cs[-to_delete_i]
    rv$gmt_cs_paths <- rv$gmt_cs_paths[-to_delete_i]
    
    #delete GMTDF after the files are removed to free up more space
    to_delete_GMTDF <- which(rv$GMTDF$name %in% input$delete_gmt)
    rv$GMTDF <- rv$GMTDF[-to_delete_GMTDF,]
    
})