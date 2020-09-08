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
        "Your species of interest:&nbsp<br/><b><i>",
        species_translate(input$selected_species),
        "</i></b><br/><br/>"
    )
    
})

# # feedback databases
# output$feedback_dbs <- renderUI({
#     req(rv$run == "success")
#     # req(rv$db_status == "selected")
#     db_selected = names(rv$dbs)
#     HTML(
#         "<br/>Selected databases:<br/><b>",
#         paste(db_selected,collapse = "; "),
#         "</b><br/><br/>"
#     )
# })

#==========================================#
#####      FEEDBACKS on GSEA run       #####
#==========================================#
# feedback example data
output$feedback_rnk <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(is.null(rv$example_file) == F)
    req(is.null(rv$infile_name) == F)
    HTML(
        "You have selected our example data. "
    )
})

# feedback file name
output$feedback_filename <- renderUI({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(is.null(rv$infile_name) == F)
    HTML(
        "Your query file:<br/><b>",
        rv$infile_name,
        "</b><br/><br/>"
    )
})

# --------------- feedback on input file content --------------
output$feedback_filecontent <- renderTable({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    req(rv$file_upload_status == "uploaded")
    req(is.null(rv$infile_confirm) == T)
    df = rv$data_head %>%
        head(.,n=2) %>%
        dplyr::mutate_if(is.numeric, function(x) round(x,2))
    
    arow = rep("...",ncol(df))
    
    rbind(df,arow)
        
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
                label = "Gene column:",
                choices = colnames(rv$data_head),
                selected = match_colnames(col_gene_names,colnames(rv$data_head))
            )
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "logfc_column",
                label = "logFC column:",
                choices = colnames(rv$data_head),
                selected = match_colnames(col_fc_names,colnames(rv$data_head))
            )
        ),
        column(
            width = 4,
            radioButtons(
                inputId = "p_column",
                label = "P column:",
                choices = colnames(rv$data_head),
                selected = match_colnames(col_p_names,colnames(rv$data_head))
            )
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
                label = "Gene column:",
                choices = colnames(rv$data_head),
                selected = match_colnames(col_gene_names,colnames(rv$data_head))
            )
        ),
        column(
            width = 6,
            radioButtons(
                inputId = "rank_column",
                label = "Rank column:",
                choices = colnames(rv$data_head),
                selected = match_colnames(col_rank_names,colnames(rv$data_head))
            )
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
                "Confirm",
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
                    "No ID detected in ",species_translate(species),"'s database. Please check if your query file is correct and/or if your selected species matches your query."
                )
            )
        }else if(rv$rnk_check == "low"){
            fluidRow(
                column(
                    width = 12,
                    br(),
                    "Converted RNK for pre-ranked GSEA run:",
                    uiOutput("converted_rnk"),
                    paste0("Total number of genes: ",
                           rv$total_genes_after, " / ",rv$total_genes),
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
                    br(),
                    "Converted RNK for pre-ranked GSEA run:",
                    uiOutput("converted_rnk"),
                    paste0("Total number of genes: ",
                           rv$total_genes_after, " / ",rv$total_genes),
                )
            )
        }
    }
})

# RNK table
output$converted_rnk <- renderTable({
    req(input$selected_mode == "gsea")
    req(rv$db_status == "selected")
    df = data.frame(GeneName=names(rv$rnkgg),Rank=rv$rnkgg,stringsAsFactors = F) %>%
        dplyr::top_n(2) %>%
        dplyr::mutate_if(is.numeric, function(x) as.character(x))
    
    if(nrow(df)>1){
        df = df %>%
            dplyr::add_row(GeneName="...",Rank="...")
    }
    
    df
})

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
                    # style="text-align:center",
                    # width = 12, status = "warning",
                    # h5(tags$b(paste0("\"",rv$rnkll,"\""))),
                    h5(tags$b("Summary Report")),
                    # br(),
                    p(paste0("Mode of analysis: ",names(run_modes[run_modes == rv$run_mode]))),
                    # br(),
                    tags$ul(
                        tags$li(HTML("Species: <b><i>",species_translate(input$selected_species),"</b></i>")),
                        tags$li(HTML("Databases: <b>",db_selected,"</b>")),
                        tags$li(HTML("Gene set size filters min=",rv$gmin," max=",rv$gmax," results in ",rv$gmts_length," / ",length(rv$gmts)," gene sets")),
                        tags$li(HTML("Number of permutation=",rv$gperm)),
                        tags$li(HTML("<b>",rv$no_down_05,"</b> (down) <b>",rv$no_up_05,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_down_01,"</b> (down) <b>",rv$no_up_01,"</b> (up) "," gene sets are significantly enriched at P.adj < 0.01"))
                    ),
                    br(),
                    HTML("Navigate to <b>Enrichment Results</b> for details.")
                )
            )
        }else if(rv$run_mode == "glist"){
            fluidRow(
                wellPanel(
                    style = paste0("background:",bcol2),
                    # style="text-align:center",
                    # width = 12, status = "warning",
                    # h5(tags$b(paste0("\"",rv$rnkll,"\""))),
                    h5(tags$b("Summary Report")),
                    # br(),
                    p(paste0("Mode of analysis: ",names(run_modes[run_modes == rv$run_mode]))),
                    # br(),
                    tags$ul(
                        tags$li(HTML("Species: <b><i>",species_translate(input$selected_species),"</b></i>")),
                        tags$li(HTML("Databases: <b>",db_selected,"</b>")),
                        tags$li(HTML("Gene set size filters min=",rv$gmin," max=",rv$gmax," results in ",rv$gmts_length," / ",length(rv$gmts)," gene sets")),
                        tags$li(HTML("<b>",rv$no_up_05,"</b> gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(HTML("<b>",rv$no_up_01,"</b> gene sets are significantly enriched at P.adj < 0.01"))
                    ),
                    br(),
                    HTML("Navigate to <b>Enrichment Results</b> for details.")
                )
            )
        }
    }else if(rv$run == "failed"){
        fluidRow(
            box(
                background = "red", width = 12,
                HTML("No enrichment results for <b>",rv$rnkll,"</b>. Please check if species matches your query and/or if your input file is correct.")
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
    
# UI summary ----------------
    output$summary_box <- renderUI({
        # req(input$summary_type=="summary"||is.null(rv$run)||rv$run != "success")
        # req(input$plot_type!="bar");req(input$plot_type!="bubble");req(input$plot_type!="volcano");
        # req(input$summary_type!="id")
        fluidPage(
            # style = "position: relative",
            fluidRow(
                # # title = "Welcome to easyGSEA",solidHeader=T,
                # width = 12,align = "left", #height = "670px",
                # status = "primary", 
                column(
                    width = 6,
                    # h5("Hello!"),
                    br(),
                    uiOutput("feedback_runmode"),
                    uiOutput("feedback_species"),
                    # uiOutput("feedback_dbs"),
                    uiOutput("feedback_rnk"),
                    uiOutput("feedback_filename"),
                    uiOutput("feedback_filecontent"),
                    # uiOutput("feedback_filecontent_deg"),
                    # uiOutput("feedback_filecontent_rnk"),
                    # uiOutput("feedback_filecontent_confirm"),
                    uiOutput("feedback_converted_rnk"),
                    
                    uiOutput("feedback_glist"),
                    uiOutput("feedback_converted_glist")
                    
                ),
                column(
                    width = 6, #offset = 1,
                    br(),
                    uiOutput("run_summary_gsea")
                )
            )
        )
        
    })

# --------- UI ID conversion ----------
output$id_box <- renderUI({
    # req(input$summary_type=="id")
    fluidPage(
        # style = "position: relative",
        fluidRow(
            width = 12,align = "left",#height = "670px",
            status = "primary",
            fluidRow(
                column(
                    width = 4,
                    uiOutput("ui_mat_download")
                ),
                column(
                    width = 4,
                    uiOutput("ui_rnk_download")
                )
            ),
            br(),
            uiOutput("id_none"),
            dataTableOutput("id_conversion_table")
        )
    )
})

# UI ID conversion table
# box to display if no ID conversion
output$id_none <- renderUI({
    req(is.null(rv$gene_lists_mat))
    
    wellPanel(
        "ID conversion table available when applicable."
    )
})

# render ID conversion table
output$id_conversion_table <- DT::renderDataTable({
    rv$gene_lists_mat
}, plugins="ellipsis",options=dt_options())

# download ID conversion button
output$ui_mat_download <- renderUI({
    req(is.null(rv$gene_lists_mat) == F)
    downloadButton("mat_download",
                   label = "Download ID conversion table (.csv)"
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
    downloadButton("rnk_download",
                   label = "Download RNK (.rnk)"
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