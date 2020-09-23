# Overall bodyNetwork UI ------------------
output$ui_bodyNetwork <- renderUI({
    if(is.null(rv$run) || rv$run != "success"){
        box(
            title = span( icon("exclamation"), "Notification"), status = "warning", width=6,
            "Visualization available upon successful run."
        )
    }else{
        fluidRow(
            # uiOutput("ui_vis_gear"),
            
            box(
                width = 12,
                #<i class="fas fa-chart-network"></i>
                title=span( icon("project-diagram"), "Network view of enriched gene sets"), status = "primary",
                uiOutput("ui_vis_gear"),
                
                div(id="d_vis",
                    style = "position: absolute; right: 1em; top: 1em;",
                    downloadBttn(
                        size = "md", style="unite",
                        outputId = "download_vis", label = NULL
                    ),
                    bsTooltip("d_vis","Click to download plot", placement = "bottom")
                ),
                div(
                    uiOutput("vis_error")
                ),
                div(
                    visNetworkOutput("vis_network", height = "660px")
                )
                
            )
        )
    }
})

# vis network--------------
output$vis_error <- renderUI({
    req(rv$vis_status == "failed")
    HTML(
        "No significant enrichment found at pval < ",
        rv$vis_p,
        " & q < ",
        rv$vis_q,
        ". Please adjust thresholds by clicking the top-right gear button."
    )

})
output$vis_network <- renderVisNetwork({
    req(is.null(rv$fgseagg)==F)
    # N = 10
    withProgress(message = 'Generating plots ...',value = 1, {
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
        style = "position: absolute; right: 5em; top: 1em;",
        dropdown(
            up = FALSE,right = TRUE,icon = icon("gear"),width = "800px",
            style = "unite",#status = "primary",#size = "sm",
            circle = TRUE,
            tooltip = tooltipOptions(
                title = "Click to adjust parameters for creating a network"
                ,placement = "bottom"),
            
    #         animate = animateOptions(
    #             enter = "slideInRight",
    #             exit = "fadeOutRight", duration = 0.5
    #         ),
    # box(
    #     width = 12,
    #     title = span(icon("gear", class = "opt"),"Advanced parameters for creating a network"), solidHeader = T,
    #     icon = "fa fa-th",
    #     status = "primary", 
    #     # solidHeader = T,
    #     collapsible = T, collapsed = T, 
            div(
                align = "center",
                tags$h4(tags$strong(tags$em("Advanced parameters for creating a network"))),br()
            ),
            fluidRow(
                column(
                    width = 5,
                    sliderTextInput("cutoff_vis_p",
                                    label = "Adjust P threshold:",
                                    choices= cutoff_slider,
                                    selected=rv$vis_p, grid=T, force_edges=T)
                ),
                column(
                    width = 5,
                    sliderTextInput("cutoff_vis_q",
                                    label = "Adjust P.adj threshold:",
                                    choices= cutoff_slider,
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
                    ),
                    bsTooltip("q_vis_edge", "Click to learn more!", placement = "top")
                ),
                column(
                    width = 5,
                    numericInput("vis_percent_cutoff",
                                 label = p("Edge threshold",
                                           tags$style(type = "text/css", "#q_vis_edge_threshold {display: inline-block;width: 20px;height: 20px;padding: 0;border-radius: 50%;vertical-align: baseline;margin-left: 10px;}"),
                                           bsButton("q_vis_edge_threshold", label = "", icon = icon("question"), style = "default", size = "extra-small")),
                                 rv$percent_cutoff, min = 0, max = 1, step = 0.01
                    ),
                    bsTooltip("q_vis_edge_threshold", "Click to learn more!", placement = "top")
                    
                )
                
            ),br(),
            fluidRow(
                column(
                    width = 5,
                    conditionalPanel(
                        condition = "input.vis_percent == 'combined'",
                        numericInput("combined_k","Combined constant, K",
                                     rv$vis_k, min = 0, max = 1, step = 0.01
                        )
                    )
                ),
                column(
                    width = 7,align="right",br(),
                    actionBttn("vis_replot","Replot!"
                               ,style = "simple"#,size = "sm"
                             ,color = "primary",icon = icon("atom") #,lib="font-awesome"
                    )
                )
            )
            
            
            
        )
    )
    
})
