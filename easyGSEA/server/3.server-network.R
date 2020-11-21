# Overall bodyNetwork UI ------------------
output$ui_bodyNetwork <- renderUI({
    if(is.null(rv$run) || rv$run != "success"){
        # add an id for introjs
        box(id = "enrichment_network_box",
            title = span( icon("exclamation"), "Notification"), status = "warning", width=6,
            "Visualization available upon successful run."
        )
    }else{
        fluidRow(
            # add an id for introjs
            box(id = "enrichment_network_box2",
                width = 7,
                #<i class="fas fa-chart-network"></i>
                title=span( icon("project-diagram"), "Network view of enriched gene sets"), status = "primary",
                
                
                div(
                    uiOutput("vis_error")
                ),
                div(
                    if(!is.null(rv$vis_status) && rv$vis_status == "max exceeded"){
                        div(
                            br(),
                            p("We support a maximum of 500 data points in Enrichment Network.",
                                    "Please adjust the P and/or P.adj thresholds by clicking the top-right gear button"),
                            br()
                        )
                    } else {
                        visNetworkOutput("vis_network", height = "660px")
                    }
                )
                ,absolutePanel(
                    fluidRow(
                        # add a id for the gear button in introjs
                        div(id = "gear_box",
                            style="display: inline-block;vertical-align:top;margin-right:5px;",
                            dropdown(
                                up = FALSE,right = TRUE,icon = icon("gear"),width = "450px",
                                style = "unite",#status = "primary",#size = "sm",
                                circle = TRUE,
                                tooltip = tooltipOptions(
                                    title = "Click to adjust parameters for creating a network"
                                    ,placement = "bottom"),
                                
                                #         animate = animateOptions(
                                #             enter = "slideInRight",
                                #             exit = "fadeOutRight", duration = 0.5
                                #         ),
                            uiOutput("ui_vis_gear"))
                        ),
                        div(id="d_vis", style="display: inline-block;vertical-align:top;margin-right:5px;",
                            
                            # style = "position: absolute; right: 1em; top: 1em;",
                            downloadBttn(
                                size = "md", style="unite",
                                outputId = "download_vis", label = NULL
                            )
                        )
                    ),
                    right = 25,
                    top = 12
                )
            ),
            box(
                id = "dendrogram_box",
                width = 5,
                title = div( #span(icon("pagelines"), #" Select the plot you would like to explore"),
                                  selectizeInput("dendro_or_barplot",
                                                 NULL,
                                                 choices = c("Cluster dendrogram"="dendro", "Cluster bar plot"="bar"),
                                                 selected = rv$dendro_or_barplot,
                                                 width = "180px")
                    
                    # ,div(style = "position: relative;bottom: 1em;",
                    #    actionBttn("dendro_or_barplot_confirm","View"
                    #            ,style = "simple",size = "sm"
                    #            ,color = "primary"
                    #         ) 
                    #     )
                    ),
                status = "primary",
                div(
                    style="overflow-y:scroll; overflow-x:scroll; max-height: 660px", #max-height:600px;
                    if(rv$dendro_or_barplot == "dendro"){
                        if(!is.null(rv$dendro_run) && rv$dendro_run == "fail"){
                            div(
                                    br(),
                                    p("At least two enriched gene sets are needed for a dendrogram view."),
                                    br()
                                )
                        } else if(!is.null(rv$vis_status) && rv$vis_status == "max exceeded"){
                                div(
                                    # br(),
                                    # p("We support a maximum of 500 data points in Enrichment Network. Please adjust adjust thresholds by clicking the top-right gear button in <b>Network view of enriched gene sets</b>."),
                                    # br()
                                )
                            }
                        else{
                                plotlyOutput("plot_dendrogram", width = "800px", height = '1320px')
                        }
                    } else {
                        if(!is.null(rv$cluster_bar_run) && rv$cluster_bar_run == "fail"){
                            div(
                                br(),
                                p("At least two enriched gene sets are needed for a bar plot view of the clusters."),
                                br()
                            ) 
                        } else if(!is.null(rv$vis_status) && rv$vis_status == "max exceeded"){
                            div(
                                # br(),
                                # p("We support a maximum of 500 data points in Enrichment Network. Please adjust adjust thresholds by clicking the top-right gear button in <b>Network view of enriched gene sets</b>."),
                                # br()
                            )
                        } else {
                            plotlyOutput("plot_cluster_bar", width = "900px", height = "660px")
                        }
                    }
                    
                )
                # ,div(id="dendro_dropdown", style = "position: absolute; left: 1em; bottom: 2em;",
                #     dropdown(
                #         uiOutput("dendro_option"),
                #         size = "xs",
                #         width = '300px',
                #         icon = icon("gear", class = "opt"),
                #         up = TRUE
                #     ) 
                # )

                ,absolutePanel(
                    # add a id for the gear button in introjs
                    div(id = "dendro_dropdown",
                        style="display: inline-block;vertical-align:top;
                            ", #position: absolute; right: 55px; top: 4em;
                        dropdown(
                            # if(rv$dendro_or_barplot == "Cluster barplot"){
                            #     uiOutput("barplot_option")
                            # } else {
                            uiOutput("dendro_option")
                            ,
                            width = '300px',
                            
                            up = FALSE,right = TRUE,icon = icon("gear"),
                            style = "unite",
                            circle = TRUE,
                            tooltip = tooltipOptions(
                                title = "Click to adjust parameters for creating a dendrogram"
                                ,placement = "bottom"),
                        )
                    ),
                    div(id="d_dendro", style="display: inline-block;vertical-align:top;
                            ", #position: absolute; top: 4em; right: 0px;
                        
                        # style = "position: absolute; right: 1em; top: 1em;",
                        downloadBttn(
                            size = "md", style="unite",
                            if(rv$dendro_or_barplot == "Cluster dendrogram"){outputId = "download_dendro"}
                            else{outputId = "download_cluster_barplot"}
                            , label = NULL
                        )
                    ),
                    
                    nav_btn_b("net_b"),
                    nav_btn_f("net_f"),
                    
                    bsTooltip("d_vis","Click to download plot", placement = "bottom"),
                    bsTooltip("d_dendro", "Click to download plot", placement = "bottom")
                    ,bsTooltip("net_b",HTML("Return to <b>Enrichment Results</b>")
                               ,placement = "bottom")
                    ,bsTooltip("net_f",HTML("Proceed to <b>Download</b>")
                               ,placement = "bottom")
                    ,
                    
                    right = 10,
                    top = 8
                )
                
            )
        )
    }
})

# ------------ nav buttons to previous/next tab -----------
observeEvent(input$net_b,{
    updateTabItems(session, "tabs", "kegg")
})

observeEvent(input$net_f,{
    updateTabItems(session, "tabs", "download")
})

# -------------- vis network--------------
output$vis_error <- renderUI({
    req(rv$vis_status == "failed")
    HTML(
        "<br><br>No significant enrichment found at pval < ",
        rv$vis_p,
        " & q < ",
        rv$vis_q,
        ". Please adjust the P and/or P.adj thresholds by clicking the top-right gear button."
    )

})
output$vis_network <- renderVisNetwork({
    req(is.null(rv$fgseagg)==F)
    # N = 10
    withProgress(message = 'Generating network view of enriched gene sets ...',value = 1, {
        rv$vis = vis()
        return(rv$vis)
    })
})

# render Plotly Dendrogram
output$plot_dendrogram <- renderPlotly({
    req(is.null(rv$fgseagg)==F)
    withProgress(message = "Hierarchically clustering enriched gene sets and generating the dendrogram ...",value = 1,{
        rv$dendro <- plot_dendro()
        return(rv$dendro)
    })
    
})

# render Plotly cluster barplot
output$plot_cluster_bar <- renderPlotly({
    req(is.null(rv$fgseagg)==F)
    withProgress(message = "Clustering enriched gene sets and generating the barplot ...",value = 1,{
        rv$cluster_barplot <- plot_cluster_barplot()
        return(rv$cluster_barplot)
    })
    
})

# the input that user selected that controls the plot displayed
observeEvent(input$dendro_or_barplot,{
    rv$dendro_or_barplot <- input$dendro_or_barplot
})

# the dropdown gear ui of dendrogram
output$dendro_option <- renderUI({
    #req(rv$dendro_run == "success")
    div(
            numericInput("dendro_cutoff", 
                         HTML(paste0("Similarity threshold = :( 0 &#x2264 x &#x2264 1 )",add_help("dendro_help",style = "top: 1px; right:0px"))),
                         value = rv$cutoff_point, min = 0, max = 1, step=0.01),
            #add_help("dendro_help",style = "position:absolute; top: 1px; right:0px"),
            bsTooltip("dendro_help", "Gene sets that have a similarity score larger than or equal to x are grouped together",placement = "bottom"),
            if(rv$dendro_or_barplot == "dendro")
                {numericInput("dendro_label_size", 
                         HTML(paste0("Label text size : ( 0 &#x2264 y &#x2264 6 )",add_help("dendro_label_size_help",style = "top: 1px; right:0px"))),
                         value = rv$label_size, min = 0, max = 6, step=0.1)},
            #add_help("dendro_label_size_help",style = "top: 1px; right:0px")),
            bsTooltip("dendro_label_size_help", "The text size of the labels (cluster id and the most significant gene set) in the dendrogram",placement = "bottom"),
            numericInput("dendro_cluster_size", 
                         HTML(paste0("Minimum Cluster size for labels = :", br(), "( 0 &lt; z &#x2264 ", rv$max_cluster_size," )",add_help("cluster_size_help",style = "top: 1px; right:0px"))),
                         value = rv$cluster_size, min = 1, max = rv$max_cluster_size, step = 1),
            bsTooltip("cluster_size_help", "The clusters that have at least z gene sets are labeled",placement = "bottom"),
            actionBttn("dendro_update","Replot!"
                       ,style = "simple",size = "sm"
                       ,color = "primary"
            )
        )

})

# The button to replot the dendrogram
observeEvent(input$dendro_update,{
    # update the corresponding RVs to replot the dendrogram
    if(!(rv$cutoff_point == input$dendro_cutoff) && (input$dendro_cutoff >= 0 && input$dendro_cutoff <= 1)){
       rv$cutoff_point = input$dendro_cutoff 
    }
    if(rv$dendro_or_barplot == "Cluster dendrogram"){
            if(!(rv$label_size == input$dendro_label_size) && input$dendro_label_size >= 0 && input$dendro_label_size <= 6){
            rv$label_size = input$dendro_label_size 
        }  
    }
    if(!(rv$cluster_size == input$dendro_cluster_size) && input$dendro_cluster_size <= rv$max_cluster_size && input$dendro_cluster_size >=0){
        rv$cluster_size = input$dendro_cluster_size
    }
    
    
})

# download dendrogram button
output$download_dendro <- downloadHandler(
    filename = function() {paste0("dendrogram_",paste0("cutoff_",rv$cutoff_point,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(rv$dendro), file, selfcontained = TRUE)}
    
)
# download cluster barplot button
output$download_cluster_barplot <- downloadHandler(
    filename = function() {paste0("barplot_cluster_",paste0("cutoff_",rv$cutoff_point,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(rv$cluster_barplot), file, selfcontained = TRUE)}
    
)


# download visnetwork
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
        footer = modalButton("OK")
    ))
    
})

observeEvent(input$q_vis_edge_threshold,{
    showModal(modalDialog(
        inputId = "vis_edge_md",
        title = "Recommendations on choice of thresholds",
        includeMarkdown(paste0(getwd(),"/inc/edge_threshold_explaination.md")),
        easyClose = TRUE,size="l",
        footer = modalButton("OK")
    ))
    
})

#  ============UI vis parameter =============
output$ui_vis_gear <- renderUI({
    # div(
    #     align = "left",
    #     style = "position: absolute; right: 5em; top: 1em;",
    # box(
    #     width = 12,
    #     title = span(icon("gear", class = "opt"),"Advanced parameters for creating a network"), solidHeader = T,
    #     icon = "fa fa-th",
    #     status = "primary", 
    #     # solidHeader = T,
    #     collapsible = T, collapsed = T, 
            div(div(
                align = "center",
                tags$h4(tags$strong(tags$em("Advanced parameters for creating a network"))),br()
            ),
            fluidRow(
                column(
                    width = 6,
                    sliderTextInput("cutoff_vis_p",
                                    label = "Adjust P threshold:",
                                    choices= cutoff_slider,
                                    selected=rv$vis_p, grid=T, force_edges=T)
                ),
                column(
                    width = 6,
                    sliderTextInput("cutoff_vis_q",
                                    label = "Adjust P.adj threshold:",
                                    choices= cutoff_slider,
                                    selected=rv$vis_q, grid=T, force_edges=T)
                )
            ),br(),
            fluidRow(
                column(
                    width = 6,
                    selectInput("vis_percent",
                        label = label_with_help_bttn("Edge parameters","q_vis_edge"),
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
                    width = 6,
                    numericInput("vis_percent_cutoff",
                                 label = label_with_help_bttn("Edge threshold","q_vis_edge_threshold"),
                                 rv$percent_cutoff, min = 0, max = 1, step = 0.01
                    ),
                    bsTooltip("q_vis_edge_threshold", "Click to learn more!", placement = "top")
                )
                
            ),br(),
            fluidRow(
                column(
                    width = 6,
                    conditionalPanel(
                        condition = "input.vis_percent == 'combined'",
                        numericInput("combined_k",HTML(paste0("Combined constant, K",add_help("q_ck"))),
                                     rv$vis_k, min = 0, max = 1, step = 0.01
                        )
                        ,bsTooltip("q_ck","Combined coefficient merges the Jaccard and Overlap coefficients. K is the proportion of Jaccard coefficient."
                                   ,placement = "top")
                    )
                )
            ),
    fluidRow(
                column(
                    width = 6,
                    radioGroupButtons("p_or_q_vis","Color by",
                                      choiceNames = c("P", "P.adj"),
                                      choiceValues = c("pval", "padj"),
                                      selected = rv$vis_pq,
                                      direction = "horizontal",status="default"
                    )
                ),
                column(
                    width = 6,align="right",br(),
                    actionBttn("vis_replot","Replot!"
                               ,style = "simple"#,size = "sm"
                               ,color = "primary",icon = icon("atom") #,lib="font-awesome"
                    )
                )
            )
            
            
            
        )
    # )
    
})
