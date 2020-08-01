# feedbacks about inputs -------------
# feedback run mode
output$feedback_runmode <- renderUI({
    HTML(
        "Mode of analysis:&nbsp<b>",
        species_translate(input$selected_mode,run_modes),
        "</b><br/><br/>"
    )
})
# feedback species
output$feedback_species <- renderUI({
    req(nchar(input$selected_species)>0)
    HTML(
        "Your species of interest:&nbsp<br/><b><i>",
        species_translate(input$selected_species),
        "</i></b><br/>"
    )
    
})

# feedback databases
output$feedback_dbs <- renderUI({
    req(rv$db_status == "selected")
    # db_selected = names(ext_collections[[input$selected_species]])[which(ext_collections[[input$selected_species]] %in% input$selected_db_ext)]
    # db_selected = c(db_selected,names(msigdb_collections)[which(msigdb_collections %in% input$selected_db)])
    db_selected = names(rv$dbs)
    HTML(
        "<br/>You have selected databases:<br/><b>",
        paste(db_selected,collapse = "; "),
        "</b><br/><br/>"
    )
})

# feedback example data
output$feedback_rnk <- renderUI({
    req(is.null(rv$example_file) == F)
    req(is.null(rv$infile_name) == F)
    HTML(
        "You have selected our example data. "
    )
})

# feedback file name
output$feedback_filename <- renderUI({
    req(is.null(rv$infile_name) == F)
    HTML(
        "Your query file:<br/><b>",
        rv$infile_name,
        "</b><br/>"
    )
})

# feedback file content
# output$feedback_filecontent <- renderUI({
#     req(is.null(rv$infile_name) == F)
#     HTML(
#         "Your query file:<br/><b>",
#         rv$infile_name,
#         "</b><br/>"
#     )
# })

# feedback on results ---------------------
output$run_summary_gsea <- renderUI({
    req(is.null(rv$run)==F)
    if(rv$run == "success"){
        if(rv$run_mode == "gsea"){
            fluidRow(
                box(
                    # style="text-align:center",
                    width = 12, status = "warning",
                    # h5(tags$b(paste0("\"",rv$rnkll,"\""))),
                    h5(tags$b("Summary Report")),
                    br(),
                    p(paste0("Mode of analysis: ",names(run_modes[run_modes == rv$run_mode]))),
                    br(),
                    tags$ul(
                        tags$li(paste0("The dataset has ",length(rv$rnkgg)," genes.")),
                        tags$li(paste0("Gene set size filters min=",rv$gmin," max=",rv$gmax," results in ",rv$gmts_length," / ",length(rv$gmts)," gene sets.")),
                        tags$li(paste0("Number of permutation=",rv$gperm,".")),
                        tags$li(paste0(rv$no_down_05," (down) ",rv$no_up_05," (up) "," gene sets are significantly enriched at P.adj < 0.05")),
                        tags$li(paste0(rv$no_down_01," (down) ",rv$no_up_01," (up) "," gene sets are significantly enriched at P.adj < 0.01"))
                    ),
                    br(),
                    p("Navigate to Enrichment Results for details.")
                )
            )
        }
    }else if(rv$run == "failed"){
        fluidRow(
            div(
                p(paste0("No enrichment results for ",rv$rnkll,". Please check if species matches or adjust parameters accordingly."))
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
        req(input$summary_type=="summary"||is.null(rv$run)||rv$run != "success")
        # req(input$plot_type!="bar");req(input$plot_type!="bubble");req(input$plot_type!="volcano");
        req(input$summary_type!="id")
        div(
            style = "position: relative",
            box(
                # title = "Welcome to easyGSEA",solidHeader=T,
                width = 12,height = "670px",align = "left",
                status = "primary", 
                column(
                    width = 5,
                    h5("Hello!"),
                    uiOutput("feedback_runmode"),
                    uiOutput("feedback_species"),
                    uiOutput("feedback_dbs"),
                    uiOutput("feedback_rnk"),
                    uiOutput("feedback_filename"),
                    uiOutput("feedback_filecontent")
                    
                ),
                column(
                    width = 6, offset = 1,
                    uiOutput("run_summary_gsea")
                )
            )
        )
        
    })

# UI ID conversion ----------
output$id_box <- renderUI({
    req(input$summary_type=="id")
    div(
        style = "position: relative",
        box(
            width = 12,height = "670px",align = "left",
            status = "primary", 
            downloadButton("rnk_download",
                           label = "Download RNK (.rnk)"
            ),
        )
    )
})

output$rnk_download <- downloadHandler(
    filename = function() {paste0(rv$rnkll,".rnk")},
    content = function(file) {
        fwrite(t(as.matrix(rv$rnkgg)), file, sep="\t", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=F)
    })