# Overall bodyNetwork UI ------------------
output$ui_bodyNetwork <- renderUI({
    if(is.null(rv$run) || rv$run != "success"){
        # add an id for introjs
        box(id = "enrichment_network_box",
            title = span( icon("exclamation"), "Notification"), status = "warning", width=6,
            "Visualization available upon successful run."
        )
    }else{
    if(input$sidebarCollapsed == TRUE){
        nav_left <- 35
    }else{
        nav_left <- 265
    }
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
                            style="display: inline-block;vertical-align:top;",
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
                        ),
                        bsTooltip("d_vis","Click to download the network view", placement = "bottom"),

                    ),

                    right = 25,
                    top = 6
                )
                ,
                
                div(
                    style="position:relative;z-index:1000",
                    fixedPanel(
                        fluidRow(
                            nav_btn_b("net_b"),
                            nav_btn_f("net_f")
                            
                            ,bsTooltip("net_b",HTML("Return to <b>Enrichment Results</b>")
                                       ,placement = "top")
                            ,bsTooltip("net_f",HTML("Proceed to <b>Download</b>")
                                       ,placement = "top")
                        ),
                        left = nav_left,
                        bottom = 25
                    )

                )
            ),
            box(
                id = "dendrogram_box",
                width = 5,
                title = div( id = "select_your_plot", #span(icon("pagelines"), #" Select the plot you would like to explore"),
                                  selectizeInput("dendro_or_barplot",
                                                 NULL,
                                                 choices = c("Cluster dendrogram"="dendro", "Cluster bar plot"="bar", "Cluster bubble plot" = "bubble","Cluster table" = "table"),
                                                 selected = rv$dendro_or_barplot,
                                                 width = "160px")

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
                                plotlyOutput("plot_dendrogram", width = "800px", height = paste0(rv$dendro_hp,"px"))
                        }
                    } else {
                        if(!is.null(rv$cluster_bar_run) && rv$cluster_bar_run == "fail"){
                            div(
                                br(),
                                p("At least two enriched gene sets are needed for clustering purposes."),
                                br()
                            )
                        } else if(!is.null(rv$vis_status) && rv$vis_status == "max exceeded"){
                            div(
                                # br(),
                                # p("We support a maximum of 500 data points in Enrichment Network. Please adjust adjust thresholds by clicking the top-right gear button in <b>Network view of enriched gene sets</b>."),
                                # br()
                            )
                        } else {
                            if(rv$dendro_or_barplot == "bar"){
                               plotlyOutput("plot_cluster_bar", width = "900px", height = paste0(rv$dendro_hp,"px"))
                            } else if(rv$dendro_or_barplot == "bubble"){
                                plotlyOutput("plot_cluster_bubble", width = "900px", height = paste0(rv$dendro_hp,"px"))
                            } else if(rv$dendro_or_barplot == "table"){
                                dataTableOutput("cluster_df")
                            }

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
                    fluidRow(
                        # add a id for the gear button in introjs
                        div(id = "dendro_dropdown",
                            style="display: inline-block;vertical-align:top;
                            ", #position: absolute; right: 55px; top: 2.8em;
                            # UI for customizable options for creating the dendrogram
                            if(rv$dendro_or_barplot == "dendro" || rv$dendro_or_barplot == "bar" || rv$dendro_or_barplot == "bubble"){
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
                            }
                            
                            
                        ),
                        div(id="d_dendro", style="display: inline-block;vertical-align:top;margin-right:15px;
                            ", #position: absolute; top: 2.8em; right: 0px;

                            # style = "position: absolute; right: 1em; top: 1em;",
                            downloadBttn(
                                size = "md", style="unite",
                                if(rv$dendro_or_barplot == "dendro"){outputId = "download_dendro"}
                                else if(rv$dendro_or_barplot == "bar"){outputId = "download_cluster_barplot"}
                                else if(rv$dendro_or_barplot == "bubble"){outputId = "download_cluster_bubble"}
                                else{outputId = "download_cluster_df"}
                                , label = NULL
                            )
                        )
                    )

                    # ,if(!is.null(rv$df_download)){
                    #     div(id = "download_df", style ="margin_right:15px",
                    #     downloadBttn("download_cluster_df", label = "Download data", size = "xs")
                    #     )
                    # }
                   ,


                    bsTooltip("d_dendro",
                              if(rv$dendro_or_barplot == "dendro"||rv$dendro_or_barplot == "bar"||rv$dendro_or_barplot == "bubble"){
                                  title = "Click to download plot"}
                              else{title = "Click to download table"}
                              , placement = "bottom")
                    ,
                    right = 10,
                    top = 6
                )

            )
            
         )
    }
})


# the input that user selected that controls the plot displayed
observeEvent(input$dendro_or_barplot,{
    rv$dendro_or_barplot <- input$dendro_or_barplot
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
        " in ",
        paste(rv$vis_pathway,collapse = ", "),
        ". Please adjust the selected database(s), and/or the P and/or P.adj thresholds by clicking the top-right gear button."
    )

})

# render visnetwork
output$vis_network <- renderVisNetwork({
    req(is.null(rv$fgseagg)==F)
    # N = 10
    withProgress(message = 'Generating network view of enriched gene sets ...',value = 1, {
        rv$vis = vis()
        return(rv$vis)
    })
})


# download visnetwork
output$download_vis <- downloadHandler(
    filename = function() {paste0("network_",paste0("q",rv$vis_q,"p",rv$vis_p,"_",rv$vis_pq,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(rv$vis), file, selfcontained = TRUE)}

    # content = function(file) {saveWidget(as_widget(vis()), file, selfcontained = TRUE)}
)

# -------- update and replot visnetwork ----------
observeEvent(input$vis_replot,{
    if(is.null(input$vis_pathway)){
        shinyalert("Please select at least 1 database to visualize.")
    }else{
        rv$vis_p = input$cutoff_vis_p
        rv$vis_q = input$cutoff_vis_q
        rv$vis_pq = input$p_or_q_vis
        rv$percent_method = input$vis_percent
        rv$percent_cutoff = input$vis_percent_cutoff
        rv$vis_k = input$combined_k
        rv$vis_status = NULL
        
        rv$vis_pathway <- input$vis_pathway
        
        rv$ora_color <- input$vis_color
        rv$up_color <- input$vis_color_up
        rv$down_color <- input$vis_color_down
    }
    
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
        tags$h4(tags$strong(tags$em("Advanced parameters for creating the network"))),br()
    ),
    fluidRow(
        column(12,
               selectizeInput("vis_pathway",
                              HTML(paste0("Select database(s) to plot ",add_help("db_vis"))),
                              choices = dbs(),
                              selected = rv$vis_pathway,
                              multiple = TRUE
                              )
               ,bsTooltip("db_vis",HTML(db_bs),placement = "top")
        ),
        column(
            width = 6,
            sliderTextInput("cutoff_vis_p",
                            label = HTML(paste0("Adjust P threshold ",add_help("p_vis"))),
                            choices= cutoff_slider,
                            selected=rv$vis_p, grid=T, force_edges=T)
        ),
        column(
            width = 6,
            sliderTextInput("cutoff_vis_q",
                            label = HTML(paste0("Adjust P.adj threshold ",add_help("q_vis"))),
                            choices= cutoff_slider,
                            selected=rv$vis_q, grid=T, force_edges=T)
        )
        ,bsTooltip("p_vis",HTML(p_bs),placement = "top")
        ,bsTooltip("q_vis",HTML(q_bs),placement = "top")
        
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
                         label = label_with_help_bttn(HTML("Edge threshold (0 &#8804 x &#8804 1)"),"q_vis_edge_threshold"),
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
                numericInput("combined_k",HTML(paste0("Combined constant, K ",add_help("q_ck"))),
                             rv$vis_k, min = 0, max = 1, step = 0.01
                )
                ,bsTooltip("q_ck","Combined coefficient merges the Jaccard and Overlap coefficients. K is the proportion of Jaccard coefficient."
                           ,placement = "top")
            )
        )
    ),
    fluidRow(
        column(
            width = 4,
            radioGroupButtons("p_or_q_vis",
                              label = HTML(paste0("Color by P or P.adj ",add_help("col_vis"))),
                              choiceNames = c("P", "P.adj"),
                              choiceValues = c("pval", "padj"),
                              selected = rv$vis_pq,
                              direction = "horizontal",status="default"
            )
            ,bsTooltip("col_vis",HTML(pq_bs),placement = "top")
        ),
        vis_col_div(),
        column(
            width = 12,align="right",br(),
            plot_confirm_btn("vis_replot","Replot!"
                       ,icon = icon("atom") #,lib="font-awesome"
                       ,block = T
            )
        )
    )



    )
    # )

})

# ------------ render Plotly Dendrogram, bar, bubbl --------------
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

# render Plotly bubble barplot
output$plot_cluster_bubble <- renderPlotly({
    req(is.null(rv$fgseagg)==F)
    withProgress(message = "Clustering enriched gene sets and generating the bubble plot ...",value = 1,{
        rv$cluster_bubble <- plot_cluster_bubble()
        return(rv$cluster_bubble)
    })

})

# the dropdown gear ui of dendrogram
output$dendro_option <- renderUI({
    #req(rv$dendro_run == "success")
    div(
            numericInput("dendro_cutoff",
                         HTML(paste0("Similarity threshold :( 0 &#x2264 x &#x2264 1 ) ",add_help("dendro_help",style = "top: 1px; right:0px"))),
                         value = rv$cutoff_point, min = 0, max = 1, step=0.01),
            #add_help("dendro_help",style = "position:absolute; top: 1px; right:0px"),
            bsTooltip("dendro_help", "Gene sets that have a similarity score larger than or equal to x are grouped together",placement = "top"),
            if(rv$dendro_or_barplot == "dendro")
                {numericInput("dendro_label_size",
                         HTML(paste0("Label text size : ( 0 &#x2264 y &#x2264 6 ) ",add_help("dendro_label_size_help",style = "top: 1px; right:0px"))),
                         value = rv$label_size, min = 0, max = 6, step=0.1)},
            #add_help("dendro_label_size_help",style = "top: 1px; right:0px")),
            bsTooltip("dendro_label_size_help", "The text size of the labels (cluster id and the most significant gene set) in the dendrogram",placement = "top"),
            numericInput("dendro_cluster_size",
                         HTML(paste0("Minimum Cluster size for labels ", br(), "( 0 &lt; z &#x2264 ", rv$max_cluster_size," ) ",add_help("cluster_size_help",style = "top: 1px; right:0px"))),
                         value = rv$cluster_size, min = 1, max = rv$max_cluster_size, step = 1),
            bsTooltip("cluster_size_help", "The clusters that have at least z gene sets are labeled",placement = "top"),
            if(rv$dendro_or_barplot == "bar" || rv$dendro_or_barplot == "bubble"){
                div(
                    radioGroupButtons(
                        inputId = "color_check",
                        label = HTML(paste0("Color by P or P.adj ",add_help("col_vis_bar"))),
                        choiceNames = c("P", "P.adj"),
                        choiceValues = c("pval", "padj"),
                        selected = rv$color_check,
                        direction = "horizontal"
                    ),
                    checkboxInput("sort_check", HTML(paste0("Sort the pathways ", add_help("sort_help", style = "top: 1px; right:0px")))),
                    checkboxInput("abbreviate_check", HTML(paste0("Abbreviate the labels  ", add_help("abbreviate_help", style = "top: 1px; right:0px")))),
                    bsTooltip("sort_help", "Sort the pathways by ES in ascending order in GSEA mode, or by -log10(p.value) in ascending order in ORA mode"),
                    bsTooltip("col_vis_bar",HTML(pq_bs),placement = "top")
                
                )
            },
            uiOutput("ui_abbreviate_length"),
            bsTooltip("abbreviate_help", "Abbreviate the labels when the texts are too long to be displayed", placement = "top"),
            plot_confirm_btn("dendro_update","Replot!",block=T)
        )

})

# The button to replot the dendrogram
observeEvent(input$dendro_update,{
    # update the corresponding RVs to replot the dendrogram
    if(!(rv$cutoff_point == input$dendro_cutoff) && (input$dendro_cutoff >= 0 && input$dendro_cutoff <= 1)){
       rv$cutoff_point = input$dendro_cutoff
    }
    if(rv$dendro_or_barplot == "dendro"){
            if(!(rv$label_size == input$dendro_label_size) && input$dendro_label_size >= 0 && input$dendro_label_size <= 6){
            rv$label_size = input$dendro_label_size
        }
    }
    if(!(rv$cluster_size == input$dendro_cluster_size) && input$dendro_cluster_size <= rv$max_cluster_size && input$dendro_cluster_size >=0){
        rv$cluster_size = input$dendro_cluster_size
    }
    if(rv$dendro_or_barplot == "bar" || rv$dendro_or_barplot == "bubble"){
        rv$abbreviate_check = input$abbreviate_check
        rv$sort_check = input$sort_check
        rv$color_check = input$color_check
        if(!is.null(input$abbreviate_length)){
            rv$abbreviate_length = input$abbreviate_length
        }
    }


})

# the input of the number of characters we like to abbreviate to
output$ui_abbreviate_length<- renderUI({
    req(input$abbreviate_check == TRUE)
    numericInput(
        inputId = "abbreviate_length",
        label = "String length",
        value = rv$abbreviate_length,min=1
    )
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

# download cluster bubble button
output$download_cluster_bubble <- downloadHandler(
    filename = function() {paste0("bubble_plot_cluster_",paste0("cutoff_",rv$cutoff_point,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(rv$cluster_bubble), file, selfcontained = TRUE)}

)



# ------------ render cluster table --------------
output$cluster_df <- DT::renderDataTable({
    req(rv$dendro_or_barplot=="table")
    req(!is.null(rv$df_download))
    
    df <- rv$df_download %>%
        mutate_if(is.numeric, function(x) round(x, digits=3))

    df_no(df, scrollY = "556px")
})

# download clustering's data frame
output$download_cluster_df <- downloadHandler(
    filename = function() {paste0("cluster_data_",paste0("cutoff_",rv$cutoff_point,"_"),rv$rnkll,".csv")},
    content = function(file) {
        df <- rv$df_download
        df[[ncol(df)]] = lapply(df[[ncol(df)]], function(x) paste(x,collapse = ";"))
        
        fwrite(df, file, row.names = T, quote=T)
    }
)
