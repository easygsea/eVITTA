# vis network--------------
output$vis_error <- renderUI({
    req(rv$vis_status == "failed")
    HTML(
        "No significant term found at P value threshold ",
        rv$vis_p,
        ", P.adj threshold ",
        rv$vis_q
    )

})
output$vis_network <- renderVisNetwork({
    req(is.null(rv$fgseagg)==F)
    N = 10
    withProgress(message = 'Generating plots ...', {
        for(i in 1:N){
            
            # Long Running Task
            Sys.sleep(.2)
            
            # Update progress
            incProgress(1/N)
        }
        rv$vis = vis()
        return(rv$vis)
    })
})

# download
output$download_vis <- downloadHandler(
    filename = function() {paste0("network_",paste0("q",rv$vis_q,"p",rv$vis_p,"_",rv$vis_pq,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(rv$vis), file, selfcontained = TRUE)}
    
    # content = function(file) {saveWidget(as_widget(vis()), file, selfcontained = TRUE)}
)

# update and replot
observeEvent(input$vis_replot,{
    rv$vis_p = input$cutoff_vis_p
    rv$vis_q = input$cutoff_vis_q
    rv$vis_pq = input$p_or_q_vis
    rv$percent_method = input$vis_percent
    rv$percent_cutoff = input$vis_percent_cutoff
    rv$vis_k = input$combined_k
    rv$vis_status = NULL
})

#  ============ vis edges modal =============
observeEvent(input$q_vis_edge,{
    showModal(modalDialog(
        inputId = "vis_edge_md",
        title = "Edge parameters: Determine the degree of gene overlap between gene sets",
        includeMarkdown(paste0(getwd(),"/inc/edge_explaination.md")),
        easyClose = TRUE,size="l",
        footer = modalButton("Dismiss")
    ))
    
})

observeEvent(input$q_vis_edge_threshold,{
    showModal(modalDialog(
        inputId = "vis_edge_md",
        title = "Recommendations on choice of thresholds",
        includeMarkdown(paste0(getwd(),"/inc/edge_threshold_explaination.md")),
        easyClose = TRUE,size="l",
        footer = modalButton("Dismiss")
    ))
    
})

#  ============UI vis parameter =============
output$ui_vis_gear <- renderUI({
    div(
        align = "left",
        style = "position: absolute; right: 4.5em; top: 3em;",
        dropdown(
            size = "xs",up = FALSE,right = TRUE,width = "850px",
            # circle = TRUE, tooltip = TRUE, label = "Advanced parameters for creating a network",
            icon = icon("gear", class = "opt"),
            div(
                align = "center",
                tags$h4(tags$strong(tags$em("Advanced parameters for creating a network"))),br()
            ),
            fluidRow(
                column(
                    width = 5,
                    sliderTextInput("cutoff_vis_p",
                                    label = "Adjust P threshold:",
                                    choices= c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1),
                                    selected=rv$vis_p, grid=T, force_edges=T)
                ),
                column(
                    width = 5,
                    sliderTextInput("cutoff_vis_q",
                                    label = "Adjust P.adj threshold:",
                                    choices= c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1),
                                    selected=rv$vis_q, grid=T, force_edges=T)
                ),
                column(
                    width = 2,
                    radioGroupButtons("p_or_q_vis","Color by",
                        choiceNames = c("P", "P.adj"),
                        choiceValues = c("pval", "padj"),
                        selected = rv$vis_pq,
                        direction = "horizontal",status="default"
                    )
                )
            ),br(),
            fluidRow(
                column(
                    width = 5,
                    selectInput("vis_percent",
                        label = p("Edge parameters",
                                tags$style(type = "text/css", "#q_vis_edge {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 10px;}"),
                                bsButton("q_vis_edge", label = "", icon = icon("question"), style = "default", size = "extra-small")),
                        
                        choices = list(
                            "Jaccard Coefficient" = "jaccard",
                            "Overlap Coefficient" = "overlap",
                            "Combined Coefficient" = "combined"
                        ),
                        selected = rv$percent_method
                    )
                ),
                column(
                    width = 3,
                    conditionalPanel(
                        condition = "input.vis_percent == 'combined'",
                        numericInput("combined_k","Combined constant, K",
                                     rv$vis_k, min = 0, max = 1, step = 0.01
                        )
                    )
                )
                
            ),br(),
            fluidRow(
                column(
                    width = 3,
                    numericInput("vis_percent_cutoff",
                                 label = p("Edge threshold",
                                           tags$style(type = "text/css", "#q_vis_edge_threshold {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 10px;}"),
                                           bsButton("q_vis_edge_threshold", label = "", icon = icon("question"), style = "default", size = "extra-small")),
                                 rv$percent_cutoff, min = 0, max = 1, step = 0.01
                    )
                ),
                column(
                    width = 2,offset = 7,br(),br(),
                    bsButton("vis_replot","Replot!",width="100%",
                             style = "danger",icon = icon("atom") #,lib="font-awesome"
                    )
                )
            )
            
            
            
        )
    )
    
})
