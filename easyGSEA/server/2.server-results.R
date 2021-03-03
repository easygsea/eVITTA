#=============================================================#
######                 ENRICHMENT RESULTS              ########
#=============================================================#
# ------------ Overall bodyResults UI ------------------
output$ui_bodyResults <- renderUI({
    if(!is.null(rv$demo_save) && rv$demo_save == "yes"){
        if(rv$run_mode == "glist"){
            variable_list <- c("bar_pathway", "bubble_pathway", "data_head_o", "db_modal", "db_status",
                               "dbs", "fgseagg", "file_upload_status", "gene_lists", "gene_lists_after",
                               "gene_lists_mat1", "gene_lists_mat2", "glist_check", "gmax", "gmin", "gmt_cs",
                               "gmt_cs_paths", "gmts", "gmts_length", "gperm", "infile_check", "infile_confirm",
                               "infile_name", "no_down_01", "no_down_05", "no_up_01", "no_up_05", "rnk_check",
                               "rnk_or_deg", "rnkgg", "rnkll", "run", "run_mode", "run_n", "sd_high", "volcano_pathway")
            for(i in seq_along(variable_list)){
                saveRDS(rv[[variable_list[i]]], file = paste0("rvs2/", variable_list[i], ".rds"))
            }
    
        } else {
            variable_list <- c("bar_pathway", "bubble_pathway", "data_head_o", "db_modal", "db_status",
                               "dbs", "fgseagg", "file_upload_status", "gene_lists", "gene_lists_after",
                               "gene_lists_mat1", "gene_lists_mat2", "glist_check", "gmax", "gmin", "gmt_cs",
                               "gmt_cs_paths", "gmts", "gmts_length", "gperm", "infile_check", "infile_confirm",
                               "infile_name", "no_down_01", "no_down_05", "no_up_01", "no_up_05", "rnk_check",
                               "rnk_or_deg", "rnkgg", "rnkll", "run", "run_mode", "run_n", "sd_high", "volcano_pathway")
            for(i in seq_along(variable_list)){
                saveRDS(rv[[variable_list[i]]], file = paste0("rvs/", variable_list[i], ".rds"))
            }
        }
    }

    if(is.null(rv$run) || rv$run != "success"){
        panel_null()
    }else{
        if(rv$plot_type=="bar" | rv$plot_type=="bubble"){
            em_w <- "15em"
        }else if(rv$plot_type=="manhattan"){
            em_w <- "4.5em"
        }else{
            em_w <- "8em"
        }
        fluidRow(
            column(
                8,
                div(id = "plot_choice_box",
                    style="display: inline-block;vertical-align:top;",
                    radioGroupButtons(
                        inputId = "plot_type",
                        choiceNames = list(span(icon("chart-bar"),"Bar plot"),span(icon("first-order-alt"),"Bubble plot"),span(icon("file-word"),"Keywords"),span(icon("braille"),"Manhattan plot"),span(icon("fire-alt"),"Volcano plot")), #,
                        choiceValues = list("bar", "bubble","word","manhattan","volcano"), #,
                        selected = rv$plot_type,
                        status = "primary",
                        size = "normal",
                        direction = "horizontal"
                    )
                )
                ,
                # Button that breifly explain P.value and p.adjusted Version 1
                div(id="p_value_div", style="display: inline-block;vertical-align:top;position: absolute; right: 1em;",
                    uiOutput("ui_p_help")
                )
            ),
            column(4,
                align="right",
                #Buttons that navigate to the last tab and the next tab. Version 1
                uiOutput("results_tab_control")
            ),
            column(
                width = 8,
                fluidRow(
                    box(
                        width = 12,height = rv$box_h_a,align = "center",
                        status = "primary",
                        div(
                            style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
                            uiOutput("plot_area")
                        )
                        ,
                        div(id = "setting_button",
                            align = "left",
                            style = "position: absolute; left: 1em; bottom: 1em;",
                            dropdown(
                                uiOutput("plot_gear"),
                                size = "xs",
                                icon = icon("gear", class = "opt"),
                                up = TRUE,width = "425px"
                            )
                        ),
                        # bsTooltip("setting_button",HTML("Click to adjust database selection, P & P.adj thresholds, and custom options for visualization")
                        #           ,placement = "bottom"),
                        if(rv$plot_type=="bar" || rv$plot_type=="bubble"){
                            div(id = "gs_search_button",
                                align = "left",
                                style = "position: absolute; left: 4.5em; bottom: 1em;",
                                dropdown(
                                    uiOutput("plot_gs_search"),
                                    size = "xs",
                                    icon = icon("search", class = "opt"),
                                    up = TRUE,width = "425px"
                                )
                            )
                        },
                        # bsTooltip("gs_search_button",HTML(paste0("Click to search, select, and visualize gene set(s) of interest with a ",rv$plot_type," plot"))
                        #           ,placement = "bottom"),
                        # # dropdown to adjust color tones
                        if(rv$plot_type=="bar" || rv$plot_type=="bubble" || rv$plot_type=="word"){
                            if(rv$plot_type=="bar" || rv$plot_type=="bubble"){
                                em_w_col <- "8em"
                            }else{
                                em_w_col <- "4.5em"
                            }
                            div(id = "ora_color_div",
                                align = "left",
                                style = sprintf("position: absolute; left: %s; bottom: 1em;",em_w_col),
                                dropdown(
                                    fluidRow(
                                        column(
                                            12,
                                            color_tone_div()
                                        )
                                    )
                                    ,
                                    size = "xs",
                                    icon = icon("palette", class = "opt"),
                                    up = TRUE,width = "200px"
                                )
                            )
                        },
                        # # dropdown to adjust db name & id display
                        if(rv$plot_type=="bar" || rv$plot_type=="bubble" || rv$plot_type=="volcano"){
                            if(rv$plot_type=="bar" || rv$plot_type=="bubble"){
                                em_w_t <- "11.5em"
                            }else{
                                em_w_t <- "4.5em"
                            }
                            div(id = "txt_div",
                                align = "left",
                                style = sprintf("position: absolute; left: %s; bottom: 1em;",em_w_t),
                                dropdown(
                                    tv_div()
                                    ,
                                    size = "xs",
                                    icon = icon("tv", class = "opt"),
                                    up = TRUE,width = "250px"
                                )
                            )
                        },
                        # bsTooltip("ora_color_div",HTML("Click to adjust the color tone")),
                        div(id = "plot_download_button",
                            style = sprintf("position: absolute; left: %s; bottom: 1em;",em_w),
                            dropdown(
                                uiOutput("plot_download"),
                                size = "xs",
                                icon = icon("download", class = "opt"),
                                up = TRUE
                            )
                        ),
                        # bsTooltip("plot_download_button",HTML("Click to download the plot")
                        #           ,placement = "bottom")
                    ),
                    column(12, id="feedback_btn_wrap",
                        uiOutput("feedback_btn")
                    )
                )
            ),
            column(
                width = 4,
                # div(
                fluidRow(id = "explore_geneset_box",
                    box(
                        # title = span(icon("search"),"Individual gene set statistics & visualization"),status="primary", #solidHeader = TRUE,
                        title = span(icon("search"),"Click plot, or manually select a gene set:"),status="primary", #solidHeader = TRUE,
                        id = "gs_es_result",
                        width = 12, #height = "300px",
                        fluidRow(
                            column(
                                width = 9,
                                uiOutput("es_plot_term")
                            ),
                            column(
                                width = 3, align = "right",
                                uiOutput("es_plot_term_confirm")
                            )
                        )
                        ,uiOutput("ui_es")
                    )
                )
            )

        )
    }
})

# change default plot type
observeEvent(input$plot_type,{
    rv$plot_type = input$plot_type
})

# change color tone in ORA's bar/bubble
observeEvent(input$ora_color,{
    rv$ora_color <- input$ora_color
})

# change color tone in GSEA's bar/bubble
observeEvent(input$up_color,{
    rv$up_color <- input$up_color
})

observeEvent(input$down_color,{
    rv$down_color <- input$down_color
})

# change db name & id displays
observeEvent(input$db_name_y,{
    rv$db_name_y <- input$db_name_y
})

observeEvent(input$db_id_y,{
    rv$db_id_y <- input$db_id_y
})

# feedbacks on no significant enrichment
sig_none <- reactive({
    req(rv$bar_p_cutoff)
    req(rv$bar_q_cutoff)
    
    if(rv$bar_error == "l" && (rv$plot_type == "bar" || rv$plot_type == "bubble")){
        HTML(
            "We support plotting up to 200 data points."
            ,"Please click the bottom-left gear button to adjust thresholds, "
            ,"or click the search button to re-select the gene sets of interest."
        )
    }else{
        HTML(
            "No significant term found at pval < ",
            rv$bar_p_cutoff,
            "& padj < ",
            rv$bar_q_cutoff,
            ". Please adjust thresholds by clicking the bottom-left gear button."
        )
    }
    
})

# ----------- plots' plots ---------
output$plot_area <- renderUI({
    if(rv$plot_type=="manhattan"){
        plotlyOutput("plot_manhattan", width = "100%", height = rv$box_h)
    }else if(rv$plot_type=="bar"){
        if(is.null(p_bar())){
            sig_none()
        }else{
            plotlyOutput("plot_bar", width = "100%", height = rv$box_h)
        }
    }else if(rv$plot_type=="bubble"){
        if(is.null(p_bubble())){
            sig_none()
        }else{
            plotlyOutput("plot_bubble", width = "100%",height = rv$box_h)
        }
    }else if(rv$plot_type=="volcano"){
        if(rv$run_mode == "gsea"){
            uiOutput("ui_volcano")

        }else if(rv$run_mode == "glist"){
            uiOutput("ui_volcano_glist")
        }
    }else if(rv$plot_type=="word"){
        if(is.null(word_plot())){
            sig_none()
        }else{
            plotlyOutput("plot_word", width = "100%",height = rv$box_h)
        }
    }
})

# ----------- plots' dropdown parameters ---------
output$plot_gear <- renderUI({
    if(input$selected_species != "other"){
        dbs = rv$dbs
    }else{
        dbs = rv$gmt_cs
    }

    if(rv$plot_type=="manhattan"){
        fluidRow(
            column(
                12,align="center",
                tags$h4(tags$strong(tags$em(paste0("Advanced parameters for creating the ",rv$plot_type," plot"))))
            ),
            column(12,
                   radioGroupButtons(
                       inputId = "p_or_q_manhattan",
                       label = HTML(paste0("Threshold by P or P.adj ",add_help("man_pq_q"))),
                       choiceNames = c("P", "P.adj"),
                       choiceValues = c("pval", "padj"),
                       selected = rv$volcano_pq,
                       direction = "horizontal"
                   )
                   ,bsTooltip("man_pq_q",HTML(pq_bs),placement = "top")
            ),
            column(
                width = 12,
                sliderTextInput("cutoff_manhattan",
                                label = HTML(paste0("Adjust P or P.adj threshold ",add_help("man_pq_c_q"))),
                                choices= cutoff_slider,
                                selected=rv$volcano_cutoff, grid=T, force_edges=T
                )
                ,bsTooltip("man_pq_c_q",HTML(man_pq_c_bs),placement = "top")
            ),
            column(
                width = 12,
                plot_confirm_btn("manhattan_confirm","Replot!",block=T)
            )
        )
    }else if(rv$plot_type=="bar"){

        fluidRow(
            column(
                12,align="center",
                tags$h4(tags$strong(tags$em(paste0("Advanced parameters for creating the ",rv$plot_type," plot"))))
            ),
            column(12,
                   selectizeInput("pathway_to_plot_bar",
                                  HTML(paste0("Select database(s) to plot ",add_help("db_bar"))),
                                  choices = dbs,
                                  selected = rv$bar_pathway,
                                  multiple = TRUE),
                   bsTooltip("db_bar",HTML(db_bs),placement = "top"),
                   uiOutput("bar_top"),

                   splitLayout(
                       sliderTextInput("cutoff_bar_p",
                                       label = HTML(paste0("Adjust P threshold ",add_help("p_bar"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_p_cutoff, grid=T, force_edges=T
                       ),
                       sliderTextInput("cutoff_bar_q",
                                       label = HTML(paste0("Adjust P.adj threshold ",add_help("q_bar"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_q_cutoff, grid=T, force_edges=T
                       )
                   ),
                   bsTooltip("p_bar",HTML(p_bs)
                             ,placement = "top"),
                   bsTooltip("q_bar",HTML(q_bs)
                             ,placement = "top")
                   
            ),
            column(
                width = 4,
                radioGroupButtons(
                    inputId = "p_or_q_bar",
                    label = HTML(paste0("Color by P or P.adj ",add_help("col_bar"))),
                    choiceNames = c("P", "P.adj"),
                    choiceValues = c("pval", "padj"),
                    selected = rv$bar_pq,
                    direction = "horizontal"
                )
                ,bsTooltip("col_bar",HTML(pq_bs),placement = "top")
            ),
            column(
                width = 5,
                radioGroupButtons(
                    inputId = "abb_bar",
                    label = HTML(paste0("Abbreviate y axis labels ",add_help("y_bar"))),
                    choiceNames = c("Yes", "No"),
                    choiceValues = c("y", "n"),
                    selected = rv$bar_abb,
                    direction = "horizontal"
                )
                ,bsTooltip("y_bar",HTML(abb_bs),placement = "top")
            ),
            column(
                width = 3,
                uiOutput("ui_bar_abb_n")
            ),
            column(
                width = 12, align = "right",
                plot_confirm_btn("bar_confirm","Replot!",block=T)
            )

        )
    }else if(rv$plot_type=="bubble"){


        fluidRow(
            column(
                12,align="center",
                tags$h4(tags$strong(tags$em(paste0("Advanced parameters for creating the ",rv$plot_type," plot"))))
            ),
            column(12,
                   selectizeInput("pathway_to_plot_bubble",
                                  HTML(paste0("Select database(s) to plot ",add_help("db_bubble"))),
                                  choices = dbs,
                                  selected = rv$bar_pathway,
                                  multiple = TRUE),
                   bsTooltip("db_bubble",HTML(db_bs),placement = "top"),
                   uiOutput("bubble_top"),
                   splitLayout(
                       sliderTextInput("cutoff_p_bubble",
                                       label = HTML(paste0("Adjust P threshold ",add_help("p_bubble"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_p_cutoff, grid=T, force_edges=T
                       ),
                       sliderTextInput("cutoff_q_bubble",
                                       label = HTML(paste0("Adjust P.adj threshold ",add_help("q_bubble"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_q_cutoff, grid=T, force_edges=T
                       )
                   )
                   ,bsTooltip("p_bubble",HTML(p_bs)
                             ,placement = "top"),
                   bsTooltip("q_bubble",HTML(q_bs)
                             ,placement = "top"),
            ),
            column(
                width = 4,
                radioGroupButtons(
                    inputId = "p_or_q_bubble",
                    label = HTML(paste0("Color by P or P.adj ",add_help("col_bubble"))),
                    choiceNames = c("P", "P.adj"),
                    choiceValues = c("pval", "padj"),
                    selected = rv$bar_pq,
                    direction = "horizontal"
                )
                ,bsTooltip("col_bubble",HTML(pq_bs),placement = "top")
            ),
            column(
                width = 5,
                radioGroupButtons(
                    inputId = "abb_bubble",
                    label = HTML(paste0("Abbreviate y axis labels ",add_help("y_bubble"))),
                    choiceNames = c("Yes", "No"),
                    choiceValues = c("y", "n"),
                    selected = rv$bar_abb,
                    direction = "horizontal"
                )
                ,bsTooltip("y_bubble",HTML(abb_bs),placement = "top")
            ),
            column(
                width = 3,
                uiOutput("ui_bubble_abb_n")
            ),
            column(
                width = 12,
                sliderInput("bubble_slider", 
                            HTML(paste0("Bubble size range ",add_help("bubble_size_q"))), 
                            min = 0.5, max = 30, step = 0.5,
                            value = c(2.5, 9.5))
                ,bsTooltip("bubble_size_q",HTML(bubble_size_bs),placement = "top")
                
            ),
            column(
                width = 12,
                plot_confirm_btn("bubble_confirm","Replot!",block = T)
            )
        )

    }else if(rv$plot_type=="volcano"){
        fluidRow(
            column(
                12,align="center",
                tags$h4(tags$strong(tags$em(paste0("Advanced parameters for creating the ",rv$plot_type," plot"))))
            ),
            column(
                width = 12,
                selectizeInput("pathway_to_plot_volcano",
                               HTML(paste0("Select database(s) to plot ",add_help("db_vol"))),
                               choices = dbs,
                               selected = rv$volcano_pathway,
                               multiple = TRUE),
                bsTooltip("db_vol",HTML(db_bs),placement = "top")
            ),
            column(
                width = 12,
                radioGroupButtons(
                    inputId = "p_or_q_volcano",
                    label = HTML(paste0("Color by P or P.adj ",add_help("col_vol"))),
                    choiceNames = c("P", "P.adj"),
                    choiceValues = c("pval", "padj"),
                    selected = rv$volcano_pq,
                    direction = "horizontal"
                )
                ,bsTooltip("col_vol",HTML(pq_bs),placement = "top")
            ),
            column(
                width = 12,
                radioGroupButtons(
                    inputId = "volcano_mode",
                    label = HTML(paste0("Mode of plots ",add_help("vol_mode_q"))),
                    choiceNames = c("Continuous", "Discrete","Static"),
                    choiceValues = c("plotly", "plotly2","ggplot"),
                    selected = rv$volcano_mode,
                    direction = "horizontal"
                )
                ,bsTooltip("vol_mode_q",HTML(vol_mode_bs),placement = "top")
            ),
            uiOutput("ui_volcano_cutoff"),
            column(
                width = 12,
                plot_confirm_btn("volcano_confirm","Replot!",block = T)
            )
        )
    }else if(rv$plot_type=="word"){

        fluidRow(
            column(
                12,align="center",
                tags$h4(tags$strong(tags$em(paste0("Advanced parameters for creating the ",rv$plot_type," plot"))))
            ),
            column(12,
                   selectizeInput("pathway_to_plot_word",
                                  HTML(paste0("Select database(s) to plot ",add_help("db_word"))),
                                  choices = dbs,
                                  selected = rv$bar_pathway,
                                  multiple = TRUE)
                   , bsTooltip("db_word",HTML(db_bs),placement = "top")
            ),
            column(12,
                   splitLayout(
                       sliderTextInput("cutoff_word_p",
                                       HTML(paste0("Adjust P threshold ",add_help("p_word"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_p_cutoff, grid=T, force_edges=T
                       ),
                       sliderTextInput("cutoff_word_q",
                                       HTML(paste0("Adjust P.adj threshold ",add_help("q_word"))),
                                       choices= cutoff_slider,
                                       selected=rv$bar_q_cutoff, grid=T, force_edges=T
                       )
                   )
                   ,bsTooltip("p_word",HTML(p_bs),placement = "top"),
                   bsTooltip("q_word",HTML(q_bs),placement = "top")
            
            ),
            column(12,
                   numericInput("n_word",
                                HTML(paste0("# of top words ",add_help("wtop_q"))),
                                rv$n_word, min=1,
                                width = "50%"
                   )
                   ,bsTooltip("wtop_q",HTML("The number of top frequently appearing words ")
                              ,placement = "top")
            ),
            column(
                width = 12, align = "right",
                plot_confirm_btn("word_confirm","Replot!",block = T)
            )
        )
    }
})

# ------------- plots' gene set selection dropdown ------------
output$plot_gs_search <- renderUI({
    fluidRow(
        column(
          12, align="center",
          tags$h4(tags$strong(tags$em(paste0("Manual selection of gene set(s) "))))
          
        ),
        column(
            12,
            pickerInput("gs_to_plot",
                        HTML(paste0("Select gene set(s) to plot ",add_help("manual_q"))),
                        choices = rv$gss,
                        selected = rv$gss_selected,
                        options = list(
                            `actions-box` = TRUE,
                            size = 10,
                            style = "btn-default",
                            `selected-text-format` = "count > 2"
                            ,`live-search` = TRUE
                        ),
                        multiple = TRUE)
            ,bsTooltip("manual_q",HTML(manual_bs),placement = "top")
        )
        ,column(
            12,align=T,
            plot_confirm_btn("gs_to_plot_confirm","Replot!",block=T)
        )
    )
})


# update Gene Sets list upon new successful run
observeEvent(rv$fgseagg,{
    # the entire gene list
    rv$gss <- rv$fgseagg[["pathway"]]
    names(rv$gss) <- sapply(rv$gss, function(x){subset_string(x)})
    
    # selected gene list for display
    df <- filter_plot_df(rv$bar_pathway,rv$bar_up,rv$bar_down,rv$bar_p_cutoff,rv$bar_q_cutoff)
    rv$gss_selected <- df[["pathway"]]
})

# update rv$gss_selected
observeEvent(input$gs_to_plot_confirm,{
    if(is.null(input$gs_to_plot) == T){
        shinyalert("Please select at least 1 gene set to plot.")
    }else{
        rv$bar_mode <- "gs"
        rv$gss_selected <- input$gs_to_plot
    }
})

# ------------- plots' download btn ids -----------
output$plot_download <- renderUI({
    if(rv$plot_type=="manhattan"){
        downloadButton(outputId = "download_manhattan", label = "Download plot")
    }else if(rv$plot_type=="bar"){
        div(
            downloadButton(outputId = "download_bar", label = "Download plot"),
            downloadButton(outputId = "download_bar_tl", label = "Download table")
        )
    }else if(rv$plot_type=="bubble"){
        div(
            downloadButton(outputId = "download_bubble", label = "Download plot"),
            downloadButton(outputId = "download_bubble_tl", label = "Download table")
        )
    }else if(rv$plot_type=="volcano"){
        downloadButton(outputId = "download_volcano", label = "Download plot")
    }else if(rv$plot_type=="word"){
        div(
            downloadButton(outputId = "download_word", label = "Download plot"),
            downloadButton(outputId = "download_word_tl", label = "Download table")
        )
    }
})

# manhattan plot --------------
observeEvent(input$manhattan_confirm,{
    rv$volcano_pq = input$p_or_q_manhattan
    rv$volcano_cutoff = input$cutoff_manhattan
})

# manhattan plot
p_man <- reactive({
    req(rv$run == "success")
    req(rv$plot_type=="manhattan")

    if(input$selected_species != "other"){
        dbs = rv$dbs
    }else{
        dbs = rv$gmt_cs
    }

    data = rv$fgseagg
    pq = rv$volcano_pq
    cutoff = rv$volcano_cutoff
    
    # add db column
    tmp <- str_split(data$pathway, "_", n=2, simplify = T)
    data <- data %>% tibble::add_column(db = tmp[,1], .before = "pathway")
    
    # determine colors
    color_n = length(dbs)
    colors = c(addalpha(brewer.pal(n = color_n, name = 'Set2')),brewer.pal(n = color_n, name = 'Set2'))

    # reorder rows according to pathway names and filter NA pq
    data = data %>% dplyr::arrange(pathway) %>%
        dplyr::filter(!(is.na(pval)))


    # Add highlight information
    data = data %>%
        dplyr::mutate(is_significant=ifelse(data[[pq]]<cutoff, "yes", "no")) %>%
        dplyr::mutate(is_significant=paste0(db,is_significant))

    # relevel
    levels_reordered = paste0(rep(dbs,2),c(rep("no",color_n),rep("yes",color_n)))
    data$is_significant = fct_relevel(data$is_significant,levels_reordered)

    # name colors and filter
    names(colors) = levels_reordered
    colors = colors[names(colors) %in% levels(data$is_significant)]

    # create data for manhattan
    data = data %>%
        # Compute gene set size
        dplyr::group_by(db) %>%
        dplyr::summarise(chr_len=n()) %>%

        # Calculate cumulative position of each gene set
        dplyr::mutate(tot=cumsum(chr_len)-chr_len) %>%
        dplyr::select(-chr_len) %>%

        # Add this info to the initial dataset
        dplyr::left_join(data, ., by=c("db"="db")) %>%
        dplyr::arrange(db) %>%
        dplyr::group_by(db) %>%
        dplyr::mutate(BPcum=1:n()+tot-1) %>%

        dplyr::select(-tot)

    X_axis = data %>% dplyr::group_by(db) %>% dplyr::summarize(center=(max(BPcum) +min(BPcum) ) / 2 )

    size_g = unlist(lapply(data[[ncol(data)-2]], function(x) length(x)))

    rv$manhattan_pathway_list = data[["pathway"]]

    if(rv$run_mode == "gsea"){
        ylimp <- abs(floor(log10(min(data[which(data[["ES"]]>0),][[pq]],na.rm=TRUE)))) + 0.1
        ylimn <- abs(floor(log10(min(data[which(data[["ES"]]<0),][[pq]],na.rm=TRUE)))) + 0.1


        text = paste0("<b>",data$pathway,"</b>\n",
                      "ES=",signif(data[["ES"]],digits=3),"; ",
                      "P=",signif(data[["pval"]],digits=3),"; ",
                      "P.adj=",signif(data[["padj"]],digits=3),"\n",
                      head(tail(colnames(data),n=3),n=1)," (",size_g,"/",data[["size"]],"): \n",addlinebreaks(data[[ncol(data)-2]])
        )

        p = data %>%
            ggplot(aes(x=BPcum,y=-sign(ES)*log10(data[[pq]]),text=text,color=is_significant,size=-log10(pval))) +
            geom_point(alpha = 0.75) +
            scale_color_manual(values = colors) +
            # facet_grid(. ~ db, space="free_x", scales="free_x", switch="x") +
            scale_x_continuous(expand = c(0,0), label = X_axis$db, breaks = X_axis$center) +
            scale_y_continuous(expand = c(0,0), limits = c(-ylimn, ylimp)) +
            scale_size_continuous(range = c(0.5,3)) +
            geom_hline(yintercept = -log10(cutoff), color = "grey40", linetype = "dashed") +
            geom_hline(yintercept = log10(cutoff), color = "grey40", linetype = "dashed") +
            labs(y=paste0("-log10(",pq,")*sign(ES)")) +
            theme_classic() +
            theme(
                strip.placement = "outside",
                strip.background = element_rect(fill=NA, colour="grey50"),
                legend.position="none",
                axis.text.x=element_text(face = "bold",size=12), #,angle=45,hjust=1,vjust=1
                axis.text.y=element_text(face = "bold",size=12),
                axis.title.x = element_blank(),
                axis.ticks = element_blank(),
                # axis.line.y = element_blank(),
                axis.line.x = element_blank()
            )
    }else{
        ylim <- abs(floor(log10(min(data[[pq]],na.rm=TRUE))))

        text = paste0("<b>",data$pathway,"</b>\n",
                      "P=",signif(data[["pval"]],digits=3),"; ",
                      "P.adj=",signif(data[["padj"]],digits=3),"\n",
                      head(tail(colnames(data),n=3),n=1)," (",size_g,"/",data[["size"]],"): \n",addlinebreaks(data[[ncol(data)-2]])
        )

        p = data %>%
            ggplot(aes(x=BPcum,y=-log10(data[[pq]]),text=text,color=db,size=-log10(pval))) +
            geom_point(alpha = 0.75) +
            scale_color_brewer(palette = "Set2") +
            # facet_grid(. ~ db, space="free_x", scales="free_x", switch="x") +
            scale_x_continuous(expand = c(0,0), label = X_axis$db, breaks = X_axis$center) +
            scale_y_continuous(expand = c(0,0), limits = c(0, ylim)) +
            scale_size_continuous(range = c(0.5,3)) +
            geom_hline(yintercept = -log10(cutoff), color = "grey40", linetype = "dashed") +
            labs(y=paste0("-log10(",pq,")")) +
            theme_classic() +
            theme(
                strip.placement = "outside",
                strip.background = element_rect(fill=NA, colour="grey50"),
                legend.position="none",
                axis.text.x=element_text(face = "bold",size=12), #,angle=45,hjust=1,vjust=1
                axis.title.x = element_blank(),
                axis.ticks = element_blank(),
                # axis.line.y = element_blank(),
                axis.line.x = element_blank()
            )
    }

    ggplotly(p, tooltip="text",source = "manhattan_plot_click") %>%
        event_register("plotly_click")
})

# render plot
output$plot_manhattan <- renderPlotly({
    req(rv$run == "success")
    req(rv$plot_type=="manhattan")

    withProgress(message = "Updating Manhattan plot ...",value = 1,{
        p_man()
    })

})

# manhattan download
output$download_manhattan <- downloadHandler(
    filename = function() {paste0("manhattan_",input$p_or_q_manhattan,input$cutoff_manhattan,"_",rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(p_man()), file, selfcontained = TRUE)}
)

# bar plot --------------
observeEvent(input$bar_confirm,{
    if(is.null(input$pathway_to_plot_bar)==T){
        shinyalert("Please select at least 1 database to plot.")
    }else{
        rv$error_par <- 0
        rv$error_par <- check_numericInput_na("n_up_bar", rv$error_par, "# of top up")
        rv$error_par <- check_numericInput_na("n_down_bar", rv$error_par, "# of top down")
        #print(rv$error_par)
        req(rv$error_par == 0)
        rv$bar_pathway = input$pathway_to_plot_bar
        rv$bar_pq = input$p_or_q_bar
        rv$bar_p_cutoff = input$cutoff_bar_p
        rv$bar_q_cutoff = input$cutoff_bar_q
        rv$bar_up = input$n_up_bar
        rv$bar_down = input$n_down_bar
        
        rv$bar_abb = input$abb_bar
        if(rv$bar_abb == "y"){if(is.null(input$abb_bar_n)){rv$bar_abb_n = 40}
            else{
                rv$error_par <- check_numericInput_na("abb_bar_n", rv$error_par, "String Length")
                req(rv$error_par == 0)
                rv$bar_abb_n = input$abb_bar_n}
            }
        
        rv$bar_mode <- "cutoff"
    }
})

# bar plot
p_bar <- reactive({
    if(rv$run_mode == "gsea"){
        bar_plot()
    }else if(rv$run_mode == "glist"){
        bar_plot2()
    }
})

output$plot_bar <- renderPlotly({
    req(rv$run == "success")
    req(rv$plot_type=="bar")
    req(is.null(rv$bar_pathway)==F)

    withProgress(message = "Updating bar plot ...",value = 1,{
        p_bar()
    })

})

# bar download
output$download_bar <- downloadHandler(
    filename = function() {paste0("bar_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(p_bar()), file, selfcontained = TRUE)}
)

output$download_bar_tl <- downloadHandler(
    filename = function() {paste0("bar_table_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".csv")},
    content = function(file) {
        df <- rv$bar_tl
        df <- collapse_last_col(df)
        fwrite(df, file, sep=",",row.names = F, quote=T)
    }
)


# bubble plot ----------
observeEvent(input$bubble_confirm,{
    if(is.null(input$pathway_to_plot_bubble)==T){
        shinyalert("Please select at least 1 database to plot.")
    }else{
        # check the inputs are not NAs
        rv$error_par <- 0
        rv$error_par <- check_numericInput_na("n_up_bubble", rv$error_par, "# of top up")
        rv$error_par <- check_numericInput_na("n_down_bubble", rv$error_par, "# of top down")
        #print(rv$error_par)
        req(rv$error_par == 0)
        rv$bar_pathway = input$pathway_to_plot_bubble
        rv$bar_pq = input$p_or_q_bubble
        rv$bar_q_cutoff = input$cutoff_q_bubble
        rv$bar_p_cutoff = input$cutoff_p_bubble
        rv$bar_up = input$n_up_bubble
        rv$bar_down = input$n_down_bubble
        
        rv$bar_abb = input$abb_bubble
        if(rv$bar_abb == "y"){if(is.null(input$abb_bubble_n)){rv$bar_abb_n = 40}
            else{
                rv$error_par <- check_numericInput_na("abb_bubble_n", rv$error_par, "String Length")
                req(rv$error_par == 0)
                rv$bar_abb_n = input$abb_bubble_n}}
        
        rv$bubble_zmin = input$bubble_slider[1]
        rv$bubble_zmax = input$bubble_slider[2]
        
        rv$bar_mode <- "cutoff"
    }
})


# bubble plot
p_bubble <- reactive({
    if(rv$run_mode == "gsea"){
        bubble_plot()
    }else if(rv$run_mode == "glist"){
        bubble_plot2()
    }
})
output$plot_bubble <- renderPlotly({
    req(rv$run == "success")
    req(rv$plot_type=="bubble")
    req(is.null(rv$bar_pathway)==F)

    withProgress(message = "Updating bubble plot ...",value = 1,{
        p_bubble()
    })
})

# bubble download
output$download_bubble <- downloadHandler(
    filename = function() {paste0("bubble_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(p_bubble()), file, selfcontained = TRUE)}
)

output$download_bubble_tl <- downloadHandler(
    filename = function() {paste0("bubble_table_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".csv")},
    content = function(file) {
        df <- rv$bar_tl
        df <- collapse_last_col(df)
        fwrite(df, file, sep=",",row.names = F, quote=T)
    }
)

# volcano plot ------------
observeEvent(input$volcano_confirm,{
    rv$volcano_pathway = input$pathway_to_plot_volcano
    rv$volcano_pq = input$p_or_q_volcano

    rv$volcano_mode=input$volcano_mode

    if(rv$volcano_mode == "plotly2" | rv$volcano_mode == "ggplot"){
        if(is.null(input$volcano_cutoff)){rv$volcano_cutoff = 0.05}else{rv$volcano_cutoff = input$volcano_cutoff}
        if(is.null(input$volcano_cutoff)){rv$volcano_cutoff = 0.01}else{rv$volcano_cutoff = input$volcano_cutoff}
    }

    # if(rv$volcano_mode == "ggplot"){
    #     if(is.null(input$volcano_top_down)){rv$volcano_top_down = 5}else{rv$volcano_top_down = input$volcano_top_down}
    #     if(is.null(input$volcano_top_up)){rv$volcano_top_up = 5}else{rv$volcano_top_up = input$volcano_top_up}
    # }
    if(rv$volcano_mode == "plotly2" | rv$volcano_mode == "plotly"){
        rv$volcano_name = paste0("volcano_",paste(rv$volcano_pathway,collapse = "-"),"_",paste0(rv$volcano_pq,"_"),rv$rnkll,"_",rv$volcano_mode,".html")
    }else if(rv$volcano_mode == "ggplot"){
        rv$volcano_name = paste0("volcano_",paste(rv$volcano_pathway,collapse = "-"),"_",paste0(rv$volcano_pq,"_"),rv$rnkll,"_",rv$volcano_mode,".pdf")
    }
})

# continuous plotly volcano
output$p1_fs_volcano <- renderPlotly({
    req(rv$volcano_mode == "plotly")
    req(rv$run == "success")
    req(rv$plot_type=="volcano")
    req(rv$run_mode == "gsea")
    req(is.null(rv$volcano_pathway)==F)

    withProgress(message = "Updating continuous volcano plot ...",value = 1,{
        rv$p_volcano = volcano_plot()
        return(rv$p_volcano)
    })

})

# discrete plotly volcano
output$p2_fs_volcano <- renderPlotly({
    req(rv$volcano_mode == "plotly2")
    req(rv$run == "success")
    req(rv$plot_type=="volcano")
    req(rv$run_mode == "gsea")
    req(is.null(rv$volcano_pathway)==F)


    withProgress(message = "Updating discrete volcano plot ...",value = 1,{
        rv$p_volcano = volcano_plot2()
        return(rv$p_volcano)
    })

})

#static ggplot volcano
output$p3_fs_volcano <- renderPlot({
    req(rv$volcano_mode == "ggplot")
    req(rv$run == "success")
    req(rv$plot_type=="volcano")
    req(rv$run_mode == "gsea")
    req(is.null(rv$volcano_pathway)==F)


    withProgress(message = "Updating labelled volcano plot ...",value = 1,{
        rv$p_volcano = volcano_plot3()
        return(rv$p_volcano)
    })

})

# volcano download
output$download_volcano <- downloadHandler(
    filename = function() {if(is.null(rv$volcano_name)){paste0("volcano_",paste(rv$volcano_pathway,collapse = "-"),"_",paste0(rv$volcano_pq,"_"),rv$rnkll,"_",rv$volcano_mode,".html")}else{rv$volcano_name}},
    content = function(file) {
        if(rv$volcano_mode=="ggplot"){
            ggsave(file,rv$p_volcano, device = "pdf", width = 10, height = 8, dpi = 300, units = "in")
            # pdf(file,onefile = TRUE)
            # print(volcano_plot3())
            # dev.off()
        }else{
            saveWidget(as_widget(rv$p_volcano), file, selfcontained = TRUE)
        }
    }
)

# no volcano for glist
output$ui_volcano_glist <- renderUI({
    req(rv$run_mode == "glist")
    p("No volcano plot available for gene list overrepresentation analysis.")
})

# keyword plot --------------
observeEvent(input$word_confirm,{
    if(is.null(input$pathway_to_plot_word)==T){
        shinyalert("Please select at least 1 database to plot.")
    }else{
        # check for input$n_word to confirm it is not NA
        rv$error_par <- 0
        rv$error_par <- check_numericInput_na("n_word", rv$error_par, "# of top words")
        req(rv$error_par == 0)
        
        rv$bar_pathway = input$pathway_to_plot_word
        rv$bar_p_cutoff = input$cutoff_word_p
        rv$bar_q_cutoff = input$cutoff_word_q
        rv$n_word = input$n_word
    }
})


# word plot
output$plot_word <- renderPlotly({
    req(rv$run == "success")
    req(rv$plot_type=="word")
    req(is.null(rv$bar_pathway)==F)

    withProgress(message = "Generating word frequency chart..,", value = 1,{
        word_plot()
    })
})

# keyword download
output$download_word <- downloadHandler(
    filename = function() {paste0("keyword_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".html")},
    content = function(file) {saveWidget(as_widget(word_plot()), file, selfcontained = TRUE)}
)

output$download_word_tl <- downloadHandler(
    filename = function() {paste0("keyword_table_",paste(rv$bar_pathway,collapse = "-"),"_",paste0("q",rv$bar_q_cutoff,"p",rv$bar_p_cutoff,"_",rv$bar_pq,"_"),rv$rnkll,".csv")},
    content = function(file) {
        df <- rv$word_tl
        fwrite(df, file, sep=",",row.names = F, quote=T)
    }
)

# --------- UI: bar --------------
# UI bar abbreviation length
output$ui_bar_abb_n <- renderUI({
    req(input$abb_bar == "y")
    div(
        numericInput(
            inputId = "abb_bar_n",
            label = HTML(paste0("String length ",add_help("len_bar"))),
            value = rv$bar_abb_n,min=1
        )
        ,bsTooltip("len_bar",HTML(len_bs),placement = "top")
    )
    
})

# UI bar top # of GSs
output$bar_top <- renderUI({
    if(rv$run_mode == "gsea"){
        div(
            splitLayout(
                numericInput("n_up_bar",
                             HTML(paste0("# of top up ",add_help("barup_q"))),
                             rv$bar_up, min=1,
                             width = "90%"),
                numericInput("n_down_bar",
                             HTML(paste0("# of top down ",add_help("bardown_q"))),
                             rv$bar_down, min=1,
                             width = "90%")
            )
            ,bsTooltip("barup_q",HTML(up_bs),placement = "top")
            ,bsTooltip("bardown_q",HTML(down_bs),placement = "top")
        )
    }else{
        div(
            numericInput("n_up_bar",
                         HTML(paste0("# of top enriched gene sets to display ",add_help("oratop_q"))),
                         rv$bar_up, min=1,
            )
            ,bsTooltip("oratop_q",HTML(oratop_bs),placement = "top")
        )
        
    }
})

# --------- UI: bubble --------------

# UI bubble abbreviation length
output$ui_bubble_abb_n <- renderUI({
    req(input$abb_bubble == "y")
    fluidRow(
        column(
            12,
            numericInput(
                inputId = "abb_bubble_n",
                label = HTML(paste0("String length",add_help("len_bubble"))),
                value = rv$bar_abb_n,min=1
            )
            ,bsTooltip("len_bubble",HTML(len_bs),placement = "top")
        )
    )
    
})

# UI bubble top # of GSs
output$bubble_top <- renderUI({
    if(rv$run_mode == "gsea"){
        fluidRow(
            column(
                12,
                splitLayout(
                    numericInput("n_up_bubble",
                                 HTML(paste0("# of top up ",add_help("bubbleup_q"))),
                                 rv$bar_up, min=1,
                                 width = "90%"),
                    numericInput("n_down_bubble",
                                 HTML(paste0("# of top down ",add_help("bubbledown_q"))),
                                 rv$bar_down, min=1,
                                 width = "90%")
                )
                ,bsTooltip("bubbleup_q",HTML(up_bs),placement = "top")
                ,bsTooltip("bubbledown_q",HTML(down_bs),placement = "top")
            )
        )
        

    }else{
        fluidRow(
            column(
                12,
                numericInput("n_up_bubble",
                             HTML(paste0("# of top enriched gene sets to display ",add_help("oratop_q_bubble"))),
                             rv$bar_up, min=1)
                ,bsTooltip("oratop_q_bubble",HTML(oratop_bs),placement = "top")
            )
        )
        

    }
})



# ----------- UI: volcano ----------------
output$ui_volcano <- renderUI({
    if(rv$volcano_mode=="plotly"){
        plotlyOutput("p1_fs_volcano",width = "100%",height = rv$box_h)
    }else if(rv$volcano_mode=="plotly2"){
        plotlyOutput("p2_fs_volcano",width = "100%",height = rv$box_h)
    }else if(rv$volcano_mode=="ggplot"){
        plotOutput("p3_fs_volcano",width = "100%",height = rv$box_h)
    }
})

# UI volcano p and q cutoffs
output$ui_volcano_cutoff <- renderUI({
    req(input$volcano_mode=="plotly2" | input$volcano_mode=="ggplot")
    column(
        width = 12,
        sliderTextInput("volcano_cutoff",
                        label = HTML(paste0("Adjust P or P.adj threshold ",add_help("vol_pq_c_q"))),
                        choices= cutoff_slider,
                        selected=rv$volcano_cutoff, grid=T, force_edges=T
        )
        ,bsTooltip("vol_pq_c_q",HTML(man_pq_c_bs),placement = "top")
    )
})

# GSEA table --------------------
output$gs_stats_tl <- DT::renderDataTable({
    # req(input$selected_es_term != "")
    req(rv$es_term)
    df = rv$fgseagg[which((rv$fgseagg)$pathway == rv$es_term)][1,]

    df = df %>% mutate_if(is.numeric, function(x) round(x, digits=3))

    df = t(df)
    # r_names <- names(df[-1,])
    # c_names = df[[2]]
    # 
    # df = tibble(df)
    # # # remove db cat
    # df = df[-1,]
    # rownames(df) = r_names
    colnames(df) = df[[1]]


    DT::datatable(df,
                  extensions=c('Scroller','Buttons'),
                  options = list(
                      # sDom  = '<"top">lrt<"bottom">ip',
                      dom = 'Bfrtip',
                      buttons = c('copy', 'pdf', 'csv'), #, 'excel', 'print'
                      scrollY = "128px",
                      scroller = TRUE,
                      scrollX=TRUE
                  ))
})

# Enrichment plot ---------------------------

output$plot_db_es <- renderPlot({
    req(rv$run == "success")
    req(rv$es_term)

    # req(input$selected_es_term != "")
    req(rv$es_term)
    enrichmentplot()
})

# download
output$download_gs_es <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_ESplot_",rv$es_term,".pdf")},
    content = function(file) {
        pdf(file,onefile = TRUE)
        print(enrichmentplot())
        dev.off()
    },
    contentType = 'image/pdf'
)

# Density plot ------------------------

output$plot_density <- renderPlot({
    req(rv$run == "success")
    # req(input$selected_es_term != "")
    req(rv$es_term)
    density_plot()
})

# download
output$download_density <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_Density_",rv$es_term,".pdf")},
    content = function(file) {
        pdf(file,onefile = TRUE)
        print(density_plot())
        dev.off()
    },
    contentType = 'image/pdf'
)

# Box plot ------------------------
output$plot_box <- renderPlot({
    req(rv$run == "success")
    req(rv$es_term)

    # req(input$selected_es_term != "")
    req(rv$es_term)
    box_plot()
})

# download
output$download_box <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_Box_",rv$es_term,".pdf")},
    content = function(file) {
        pdf(file,onefile = TRUE)
        print(box_plot())
        dev.off()
    },
    contentType = 'image/pdf'
)

# Violin plot ------------------------
output$plot_violin <- renderPlot({
    req(rv$run == "success")
    req(rv$es_term)

    # req(input$selected_es_term != "")
    req(rv$es_term)
    rv$k = input$cutoff_k
    violin_plot()
})

# download
output$download_violin <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_Violin_",rv$es_term,".pdf")},
    content = function(file) {
        pdf(file,onefile = TRUE)
        print(violin_plot())
        dev.off()
    },
    contentType = 'image/pdf'
)


# ES UI ----------------------------------------------------
output$ui_es <- renderUI({
    req(rv$es_term)

    fluidRow(
        column(
            width = 12
            ,div(id="ui_gsea_plots_radio_wrap",
                style = "position: relative",
                uiOutput("ui_gsea_plots"),
                br(),
                uiOutput("ui_gsea_plots_radio")
            )
            ,div(
                style = 'overflow-x: scroll;font-size:75%',
                DT::dataTableOutput("gs_stats_tl")
            )

        )

    )
})

# ES UI only when mode == gsea
output$ui_gsea_plots <- renderUI({
    req(rv$run_mode=="gsea")
    req(rv$es_term)

    div(
        style = "position: relative",
        uiOutput("gs_enrichment_plot"),
        uiOutput("density_plot"),
        uiOutput("box_plot"),
        uiOutput("violin_plot")
    )
})

output$ui_gsea_plots_radio <- renderUI({
    req(rv$run_mode=="gsea")

    fluidRow(
        column(
            width = 12, align = "center",
            radioGroupButtons(
                inputId = "plot_type_2",
                # label = "Select plot type",
                choiceNames = c("Enrichment", "Density","Box","Violin"),
                choiceValues = c("enrichment", "density", "box","violin"),
                selected = "enrichment",
                checkIcon = list(
                    yes = icon("check-square"),
                    no = icon("square-o")
                ),
                status = "primary",
                direction = "horizontal"
            ),
            tags$hr(style="border-color: grey; margin:0px;"),br()
        )

    )


})



# Observe click event pass to rv$es_term -----------
clear_plot_rv <- function(){
    rv$kegg_status <- NULL
    rv$kegg_status_g <- NULL

    rv$kegg_yes <- NULL
    rv$kegg_confirm <- NULL

    rv$reactome_yes <- NULL
    rv$reactome_confirm<-NULL

    rv$wp_yes <- NULL
    rv$wp_confirm<-NULL
}

# manhattan click
observeEvent(event_data("plotly_click", source = "manhattan_plot_click"),{
    clickData <- event_data("plotly_click", source = "manhattan_plot_click")
    # print(clickData)
    rv$es_term = rv$manhattan_pathway_list[round(clickData$x + 1)]
    # print(rv$es_term)
    # rv$es_term = clickData$y
    clear_plot_rv()
})

# bar click
observeEvent(event_data("plotly_click", source = "bar_plot_click"),{
    clickData <- event_data("plotly_click", source = "bar_plot_click")
    # print(clickData)
    rv$es_term = rv$bar_pathway_list[round(clickData$y)]
    # print(rv$es_term)
    # rv$es_term = clickData$y
    clear_plot_rv()
})

# bubble click
observeEvent(event_data("plotly_click", source = "bubble_plot_click"),{
    clickData <- event_data("plotly_click", source = "bubble_plot_click")
    rv$es_term = rv$bubble_pathway_list[round(clickData$y)]

    clear_plot_rv()
})

# full volcano click
observeEvent(event_data("plotly_click", source = "volcano_plot_click"),{
    clickData <- event_data("plotly_click", source = "volcano_plot_click")
    # print(str(clickData))
    rv$es_term = rv$volcano_pathway_list[clickData$pointNumber + 1]
    # print(str(rv$es_term))
    clear_plot_rv()
})

# discrete volcano click
observeEvent(event_data("plotly_click", source = "volcano_plot_click2"),{
    clickData <- event_data("plotly_click", source = "volcano_plot_click2")
    # print(str(clickData))
    rv$es_term = rv$volcano_pathway_list[clickData$pointNumber + 1]
    # print(str(rv$es_term))
    clear_plot_rv()
})

observeEvent(rv$es_term,{
    do.call(file.remove, list(list.files(paste0(getwd(),"/www/"),full.names = TRUE)[grepl("pdf$|xml$|png$",list.files(paste0(getwd(),"/www/")))]))

    clear_plot_rv()
    if((startsWith(rv$es_term,"KEGG"))==TRUE){rv$kegg_yes = "yes"}
    if((startsWith(rv$es_term,"RA_"))==TRUE){rv$reactome_yes = "yes"}
    if((startsWith(rv$es_term,"WP_"))==TRUE){rv$wp_yes = "yes"}


    if(rv$run_mode == "gsea"){
        x <- toupper(rv$gmts[rv$es_term][[1]])

        ranks <- rv$rnkgg
        names(ranks) = toupper(names(ranks))

        ranks2 <- ranks[x]
        ranks2 <- ranks2[!is.na(ranks2)]
        # rv$rnkgg2 <- ranks2

        r1 <- data.frame(x=c(rep("All genes",length(rv$rnkgg))),y=rv$rnkgg);row.names(r1) <- NULL
        r2 <- data.frame(x=c(rep("Genes in gene set",length(ranks2))),y=ranks2);row.names(r2) <- NULL
        rv$rr <- rbind(r1,r2);r1=NULL;r2=NULL
    }
})

# UI enrichment plot ----------------------
output$gs_enrichment_plot <- renderUI({
    req(input$plot_type_2=="enrichment")
    div(
        # status="primary",solidHeader = TRUE,
        # width = NULL, height = "300px",
        # title = "Enrichment Plot",
        plotOutput("plot_db_es", height = "246px"),
        div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em",
            dropdown(
                downloadButton(outputId = "download_gs_es", label="Download plot"),
                size = "xs",
                icon = icon("download",class="opt"),
                up = TRUE
            )
        )
    )
})

# UI density plot ----------------------

output$density_plot <- renderUI({
    req(input$plot_type_2=="density")
    div(
        # status="primary",solidHeader = TRUE,
        # width = NULL, height = "300px",
        # title = "Density Plot",
        plotOutput("plot_density", height = "246px"),
        div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em",
            dropdown(
                downloadButton(outputId = "download_density", label="Download plot"),
                size = "xs",
                icon = icon("download",class="opt"),
                up = TRUE
            )
        )
    )
})

# UI box plot ----------------------

output$box_plot <- renderUI({
    req(input$plot_type_2=="box")
    div(
        # status="primary",solidHeader = TRUE,
        # width = NULL, height = "300px",
        # title = "Box Plot",
        plotOutput("plot_box", height = "246px"),
        div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em",
            dropdown(
                downloadButton(outputId = "download_box", label="Download plot"),
                size = "xs",
                icon = icon("download",class="opt"),
                up = TRUE
            )
        )
    )
})

# UI violin plot ----------------------

output$violin_plot <- renderUI({
    req(input$plot_type_2=="violin")
    div(
        # id = "density_plot",status="primary",solidHeader = TRUE,
        # width = NULL, height = "300px",
        # title = "Violin Plot",
        plotOutput("plot_violin", height = "246px"),
        div(
            style = "position: absolute; left: 0.5em; bottom: 0.5em",
            dropdown(
                sliderTextInput("cutoff_k",
                                label = "Adjust # of standard deviation(s)",
                                choices= c(0,0.5,1,1.5,2,2.5,3),
                                width = "220px",
                                selected=rv$k, grid=T, force_edges=T
                                ),
                size = "xs",
                icon = icon("gear", class = "opt"),
                up = TRUE
            )
        ),
        div(
            style = "position: absolute; left: 4em; bottom: 0.5em",
            dropdown(
                downloadButton(outputId = "download_violin", label="Download plot"),
                size = "xs",
                icon = icon("download",class="opt"),
                up = TRUE
            )
        )

    )
})



# ------------- UI KEGG/Reactome/WP confirm btn -----------
output$feedback_btn <- renderUI({
    req(is.null(rv$kegg_yes) == F | is.null(rv$reactome_yes) == F | is.null(rv$wp_yes) == F)

    msg = paste0("Click and scroll down to visualize <b>",rv$es_term,"</b>")

    if(is.null(rv$kegg_yes) == F && rv$kegg_yes == "yes"){
        bid <- "confirm_kegg_plot"
    }else if(is.null(rv$reactome_yes) == F && rv$reactome_yes == "yes"){
        bid <- "confirm_reactome_plot"
    }else if(is.null(rv$wp_yes) == F && rv$wp_yes == "yes"){
        bid <- "confirm_wp_plot"
    }

    path_box(bid,msg)
})

# UI KEGG ---------------------

# # KEGG feedback
# output$kegg_feedback <- renderUI({
#     req(rv$kegg_yes == "yes")
#     # box(
#     #     title = NULL, background = "orange", solidHeader = TRUE, width=12,
#     #     column(
#     #         width = 10,
#     #         tags$b(paste0("Visualize KEGG pathway for \"",rv$es_term,"\"? ")),
#     #     ),
#     #     column(
#     #         width = 2, align = "right",
#     #         bsButton("confirm_kegg_plot",tags$b("YES!"),style = "danger")
#     #     )
#     # )
#     msg = paste0("Click and scroll down to visualize KEGG diagram <b>",rv$es_term,"</b>")
#     path_box("confirm_kegg_plot",msg)
# })

# KEGG feedback confirm
observeEvent(input$confirm_kegg_plot,{
    rv$kegg_confirm = "yes"
    rv$kegg_status = NULL
    rv$kegg_status_g = NULL

    do.call(file.remove, list(list.files(paste0(getwd(),"/www/"),full.names = TRUE)[grepl("pdf$|xml$|png$",list.files(paste0(getwd(),"/www/")))]))
})

# KEGG panel
    output$kegg_panel_ui <- renderUI({
        req(rv$kegg_confirm == "yes")


    #     if(is.null(rv$kegg_yes)==T){
    #         box(title="Notification", status="warning",
    #             "KEGG map is only available for KEGG gene sets.."
    #         )
    #     }
    #     else if(rv$kegg_yes == "yes"){
            fluidRow(
                    column(
                        width = 12, #offset = 7,
                        radioGroupButtons(
                            inputId = "kegg_type",
                            # label = "Select plot type",
                            choiceNames = c("KEGG native view", "KEGG graphviz layout"),
                            choiceValues = c("native","graphviz"),
                            selected = "native",
                            checkIcon = list(
                                yes = icon("check-square"),
                                no = icon("square-o")
                            ),
                            status = "primary",
                            direction = "horizontal"
                        )
                    ),
                    column(
                        width = 12,
                        uiOutput("ui_kegg_1")
                        # ,uiOutput("ui_kegg_2")
                    )
            )
    #     }
    })


    # KEGG native layout
    output$ui_kegg_1 <- renderUI({
        req(input$kegg_type)
        
        if(input$kegg_type == "native"){
            title <- span(icon("dna"),"KEGG native view")
            oid <- imageOutput("kegg_output_1")
            did <- "download_kegg_1"
        }else if(input$kegg_type == "graphviz"){
            title <- span(icon("magnet"),"KEGG graphviz layout")
            oid <- htmlOutput("kegg_output_2")
            did <- "download_kegg_2"
        }
        
        box(
            title = title,
            solidHeader = F, status = "primary",width="100%",height=500,
            scroll_up_button(),
            div(
                style="margin: 0;padding: 0; overflow-y:scroll; overflow-x:scroll",
                # add a scrolling to the plot feature
                src(),
                oid
            ),
            div(
                style = "position: absolute; left: 0.5em; bottom: 0.5em",
                dropdown(
                    downloadButton(outputId = did, label="Download plot"),
                    size = "xs",
                    icon = icon("download",class="opt"),
                    up = TRUE
                )
            )
        )
    })

    # # KEGG graphviz layout
    # output$ui_kegg_2 <- renderUI({
    #     req(input$kegg_type == "graphviz")
    #     
    #     box(
    #         title = span(icon("magnet"),"KEGG graphviz layout"),
    #         solidHeader = F, status = "primary",width="100%",height=500,
    #         scroll_up_button(),
    #         
    #         div(
    #             style="margin: 0;padding: 0; overflow-y:scroll; overflow-x:scroll",
    #             # add a scrolling to the plot feature
    #             src(),
    #             htmlOutput("kegg_output_2")
    #         ),
    #         div(
    #             style = "position: absolute; left: 0.5em; bottom: 0.5em",
    #             dropdown(
    #                 downloadButton(outputId = "download_kegg_2", label="Download plot"),
    #                 size = "xs",
    #                 icon = icon("download",class="opt"),
    #                 up = TRUE
    #             )
    #         )
    #     )
    # })








    #KEGG plot ----------------------------------------------
    # pathview draw KEGG native view
    output$kegg_output_1 <- renderImage({
        # req(is.null(rv$kegg_yes)==FALSE)
        req(input$kegg_type == "native")
        req(rv$kegg_confirm == "yes")
        #
        # if(rv$demo_mode == "gsea" && rv$es_term == "KEGG_Viral_protein_interaction_with_cytokine_and_cytokine_receptor%hsa04061"){
        #     rv$png_path = paste0(getwd(),"/www/demo/gsea.kegg.png")
        # }else{
            if(is.null(rv$kegg_status) == T){
                N = 10
                withProgress(message = paste0("Generating KEGG native view for ",rv$es_term,"..."),value = 1,{
                    # read in ranks
                    if(rv$run_mode == "gsea"){
                        ranks = rv$rnkgg
                    }else if(rv$run_mode == "glist"){
                        ranks = rep(1,length(rv$gene_lists))
                        names(ranks) = rv$gene_lists
                    }

                    # # rename to ACC, can use org.db package to change later
                    # names(ranks) = rv$input_mat[["target"]][rv$input_mat[["name"]] %in% names(ranks)]

                    # species name
                    species <- isolate(input$selected_species)

                    # KEGG gene set name & id
                    term = rv$es_term
                    kegg_id = unlist(strsplit(term,"%"))
                    kegg_id = kegg_id[length(kegg_id)]



                    if(rv$run_mode=="gsea"){
                        kegg_output <- pathview(gene.data  = ranks,
                                                pathway.id = kegg_id,
                                                species    = species,
                                                gene.idtype= "SYMBOL",
                                                kegg.dir = paste0(getwd(),"/www/"),
                                                # limit      = list(gene=max(abs(ranks))), # list(gene=c(min(ranks),max(ranks))),
                                                key.pos    = rv$kegg_pos,
                                                low = "blue", mid = "grey", high = "red", #bins = 20,
                                                kegg.native=TRUE)

                    }else if(rv$run_mode=="glist"){
                        kegg_output <- pathview(gene.data  = ranks,
                                                pathway.id = kegg_id,
                                                species    = species,
                                                gene.idtype= "SYMBOL",
                                                kegg.dir = paste0(getwd(),"/www/"),
                                                # limit      = list(gene=max(abs(ranks))), # list(gene=c(min(ranks),max(ranks))),
                                                plot.col.key = FALSE,
                                                low = "green", mid = "green", high = "green", bins = 1,
                                                kegg.native=TRUE)

                    }

                    rv$kegg_file_png = paste0(kegg_id,".pathview.png")

                    if(file.exists(rv$kegg_file_png)==T){
                        cm = paste0("mv ", rv$kegg_file_png," ",getwd(),"/www/")
                        # print(str(cm))
                        system(cm)

                        rv$png_path = paste0(getwd(),"/www/",rv$kegg_file_png)
                        rv$kegg_status = "plotted"
                    }else{
                        return(NULL)
                    }
                })

            }

        # }


        return(list(
            src = rv$png_path,
            width="100%",
            align="center",
            contentType = "image/png"
        ))
    }, deleteFile = FALSE)

    # download KEGG native
    output$download_kegg_1 <- downloadHandler(
        filename <- function() {
            paste0(rv$rnkll,"_",rv$es_term,".png")
        },
        content <- function(file) {
            file.copy(paste0(getwd(),"/www/",rv$kegg_file_png), file)
        },
        contentType = "image/png"
    )

    # pathview draw graphviz view
    output$kegg_output_2 <- renderUI({
        req(input$kegg_type == "graphviz")
        # req(is.null(rv$kegg_yes)==FALSE)
        req(rv$kegg_confirm == "yes")

        if(is.null(rv$kegg_status_g)==T){
            N = 10
            withProgress(message = paste0("Generating KEGG graphviz view for ",rv$es_term,"..."),value = 1,{

                # read in ranks
                if(rv$run_mode == "gsea"){
                    ranks = rv$rnkgg
                }else if(rv$run_mode == "glist"){
                    ranks = rep(1,length(rv$gene_lists))
                    names(ranks) = rv$gene_lists
                }

                # # rename to ACC, can use org.db package to change later
                # names(ranks) = rv$input_mat[["target"]][rv$input_mat[["name"]] %in% names(ranks)]

                # species name
                species <- isolate(input$selected_species)

                # KEGG gene set name & id
                term = rv$es_term
                kegg_id = unlist(strsplit(term,"%"))[[2]]
                # print(str(kegg_id))

                if(rv$run_mode == "gsea"){
                    kegg_output <- pathview(gene.data  = ranks,
                                            pathway.id = kegg_id,
                                            species    = species,
                                            gene.idtype= "SYMBOL",
                                            kegg.dir = paste0(getwd(),"/www/"),
                                            # limit      = list(gene=max(abs(ranks))), #list(gene=c(min(ranks),max(ranks))),
                                            key.pos    = rv$kegg_pos,
                                            low = "blue", mid = "grey", high = "red", #bins = 20,
                                            kegg.native=FALSE)
                }else if(rv$run_mode == "glist"){
                    kegg_output <- pathview(gene.data  = ranks,
                                            pathway.id = kegg_id,
                                            species    = species,
                                            gene.idtype= "SYMBOL",
                                            kegg.dir = paste0(getwd(),"/www/"),
                                            # limit      = list(gene=max(abs(ranks))), #list(gene=c(min(ranks),max(ranks))),
                                            plot.col.key = FALSE,
                                            low = "green", mid = "green", high = "green", bins = 1,
                                            kegg.native=FALSE)
                }


                rv$kegg_file_pdf = paste0(kegg_id,".pathview.pdf")

                if(file.exists(rv$kegg_file_pdf)==T){
                    cm = paste0("mv ", rv$kegg_file_pdf," ",getwd(),"/www/")
                    # print(str(cm))
                    system(cm)

                    rv$kegg_status_g = "plotted"


                }else{
                    return(paste0("KEGG graphviz view unavailable for ",rv$es_term))
                }
            })
        }
        tags$iframe(style="height:450px; width:100%", src=rv$kegg_file_pdf) #,rv$kegg_file_pdf
    })


    # download KEGG pdf
    output$download_kegg_2 <- downloadHandler(
        filename <- function() {
            paste0(rv$rnkll,"_",rv$es_term,".pdf")
        },
        content <- function(file) {
            file.copy(paste0(getwd(),"/www/",rv$kegg_file_pdf), file)
        },
        contentType = "image/pdf"
    )

    # ==============Manual selection of ES term==============
    # UI text input
    output$es_plot_term <- renderUI({
        req(rv$run == "success")
        selectizeInput(
            "selected_es_term", label = NULL,
            # "Click plot, or manually select a gene set:",
            # choices = isolate(sort_gs_terms()),
            choices = isolate((rv$fgseagg)$pathway),
            options = list(
                placeholder = 'Type to search ...',
                onInitialize = I('function() { this.setValue(""); }')
            )
        )
    })

    # UI confirm
    output$es_plot_term_confirm <- renderUI({
        req(rv$run == "success")
        # req(input$selected_es_term != "")
        div(
            actionBttn(
                "plot_db_es_confirm",
                "View!",
                style = "simple", color="primary", size = "sm",
                block = TRUE),
            bsTooltip("plot_db_es_confirm", HTML("Select term, then click <b>View!</b> Or, click interactive plots on the left"))
        )
    })

    # read in ES text input
    observeEvent(input$plot_db_es_confirm,{
        rv$es_term <- input$selected_es_term
    })

# UI REACTOME =======================
    # # REACTOME feedback
    # output$reactome_feedback <- renderUI({
    #     req(rv$reactome_yes == "yes")
    #     # box(
    #     #     title = NULL, background = "orange", solidHeader = TRUE, width=12,
    #     #     column(
    #     #         width = 10,
    #     #         tags$b(paste0("Visualize Reactome pathway for \"",rv$es_term,"\"? ")),
    #     #     ),
    #     #     column(
    #     #         width = 2, align = "right",
    #     #         bsButton("confirm_reactome_plot",tags$b("YES!"),style = "danger")
    #     #
    #     #     )
    #     # )
    #     msg = paste0("Click and scroll down to visualize Reactome diagram <b>",rv$es_term,"</b>")
    #     path_box("confirm_reactome_plot",msg)
    # })

    # reactome feedback confirm
    observeEvent(input$confirm_reactome_plot,{
        rv$reactome_confirm = "yes"
    })

    # reactome widget
    output$ui_reactome <- renderUI({
        req(rv$reactome_confirm == "yes")

        reactome_id = unlist(strsplit(rv$es_term,"%"))[2]
        reactome_genes = rv$fgseagg[rv$fgseagg$pathway==rv$es_term,][[ncol(rv$fgseagg)]]

        # if too many genes, select the top 50, otherwise bug in visualizing the pathway
        if(length(reactome_genes[[1]])>50){
            reactome_genes[[1]] = reactome_genes[[1]][1:50]
        }

        box(
            title = span(icon("lightbulb"),"Reactome Pathway Diagram"),
            solidHeader = F, status = "primary",width="100%",height=610,align = "center",
            div(
                id="diagramHolder",style = 'overflow-x: scroll',
                # add a scrolling to the plot feature
                src(),
                scroll_up_button(),
                
                tags$script(HTML(sprintf("onReactomeDiagramReady('%s','%s');",reactome_id,reactome_genes)))
            )
        )

    })

# UI WikiPathways =======================
    # WP feedback
    output$wp_feedback <- renderUI({
        req(rv$wp_yes == "yes")
        # box(
        #     title = NULL, background = "orange", solidHeader = TRUE, width=12,
        #     column(
        #         width = 10,
        #         tags$b(paste0("Visualize WikiPathways diagram for \"",rv$es_term,"\"? ")),
        #     ),
        #     column(
        #         width = 2, align = "right",
        #         bsButton("confirm_wp_plot",tags$b("YES!"),style = "danger")
        #
        #     )
        # )
        msg = paste0("Click and scroll down to visualize WikiPathways diagram <b>",rv$es_term,"</b>")
        path_box("confirm_wp_plot",msg)
    })

    # WP feedback confirm
    observeEvent(input$confirm_wp_plot,{
        rv$wp_confirm = "yes"

    })

    # WP widget
    output$ui_wp <- renderUI({
        req(rv$wp_confirm == "yes")

        wp_id = unlist(strsplit(rv$es_term,"%"))[2]

        if(rv$run_mode == "gsea" || rv$demo_mode == "gsea"){
            # get all genes under a pathway
            wp_genes = unname(unlist(rv$gmts[rv$es_term]))

            # all scale transformed ranks
            ranks = rv$rnkgg

            # get rank scores for genes in that pathway in that sample
            ranks = ranks[names(ranks) %in% wp_genes]

            # get appropriate upper/lower case
            wp_genes = wp_genes[toupper(wp_genes) %in% toupper(names(ranks))]

            # transform into appropriate format accepted by WP
            wp_genes = paste(paste("&label[]=",wp_genes,sep=""),collapse = "")

            # # coloring the nodes
            # # 1) sd transformation method
            # ii = cut(ranks, breaks = seq(-rv$sd_high,rv$sd_high,len=9), include.lowest = TRUE)
            # wp_colors  = c("darkblue","blue","lightblue","grey","grey","pink","red","brown")[ii]
            # wp_colors = paste(wp_colors,collapse = ",")

            # coloring the nodes
            # 2) step wise method
            ranks1 = ranks[ranks < 0]
            ranks2 = ranks[ranks >= 0]
            ii = cut(ranks1, breaks = c(min(ranks),seq(-rv$sd_high, 0, len = 6)), include.lowest = TRUE)
            jj = cut(ranks2, breaks = c(seq(0, rv$sd_high, len = 6),max(ranks)), include.lowest = TRUE)
            wp_colors1 = c("navy","steel","blue","lightblue","grey","grey")[ii]
            wp_colors2 = c("grey","grey","pink","salmon","red","brown")[jj]
            wp_colors = paste(c(wp_colors1,wp_colors2),collapse = ",")

            # # coloring the nodes
            # # 3) leadingedge only
            # df = rv$fgseagg
            # ES = df[df$pathway==rv$es_term,][["ES"]]
            # print(ES)
            # wp_genes = df[df$pathway==rv$es_term,][[ncol(df)]]
            #
            # wp_genes = rv$gmts[rv$es_term][[1]][toupper(rv$gmts[rv$es_term][[1]]) %in% wp_genes]
            # print(str(wp_genes))
            #
            # wp_genes = paste(paste("&label[]=",wp_genes,sep=""),collapse = "")
            # print(str(wp_genes))
            #
            # wp_colors = ifelse(ES>0,"red","blue")
            # print(wp_colors)

            rv$wp_src = sprintf("https://www.wikipathways.org/wpi/PathwayWidget.php?id=%s%s&colors=%s",wp_id,wp_genes,wp_colors)

        }else if(rv$run_mode == "glist"){
            df = rv$fgseagg
            wp_genes = df[df$pathway==rv$es_term,][[ncol(df)]][[1]]
            wp_genes = rv$gmts[rv$es_term][[1]][toupper(rv$gmts[rv$es_term][[1]]) %in% wp_genes]

            # replicate green colors to the # of genes
            wp_colors = rep("green",length(wp_genes)) %>% paste(.,collapse = ",")

            # reformat genes into acceptable format
            wp_genes = paste(paste("&label[]=",wp_genes,sep=""),collapse = "")

            rv$wp_src = sprintf("https://www.wikipathways.org/wpi/PathwayWidget.php?id=%s%s&colors=%s",wp_id,wp_genes,wp_colors)
        }
        
        
        box(
            title = span(icon("seeding"),"WikiPathways Diagram"),
            solidHeader = F, status = "primary",width="100%",height=610,align = "center",
            div(
                # add a scrolling to the plot feature
                src(),
                scroll_up_button(),
                
                tags$iframe(src = rv$wp_src, width="100%", height="550px", style="overflow:hidden;")
            )
        )

    })

    # --------------- help button for p.value and p.adj --------------
    output$ui_p_help <- renderUI({
        # req(rv$run_mode == "gsea")
        div(actionBttn(inputId="p_value_help",
                   # align="right",
                   #
                   # tags$h3("Enter genes"),
                   #
                   #
                   # textAreaInput("p_value_explanation",
                   #               "Enter genes of interest (separated by new line):",
                   #               placeholder="efk-1\nzip-2\ncep-1",
                   #               value="efk-1\nzip-2\ncep-1"
                   # ),
                   # icon = icon("font"),
                   icon = icon("product-hunt"),
                   style = "material-circle",
                   size = "md"
                   # color = ""
                   #status = "default", width = "250px",right=T,
        ),
        bsTooltip("p_value_help","Click to learn tips on choosing P & P.adj thresholds"))

    })


    observeEvent(input$p_value_help,{
        showModal(modalDialog(
            inputId = "p_help_modal",
            title = "Suggested P and P.adj thresholds for ORA and pre-ranked GSEA runs",
            includeMarkdown(paste0(getwd(),"/inc/p_explaination.md")),
            easyClose = TRUE,size="m"
            ,footer = modalButton("OK")
        ))
    })

  # create two buttons, the left arrow go to the last tab while the right arrow go to the next tab
  # version 1
    output$results_tab_control <- renderUI({
        div(nav_btn_b("results_b"),
            nav_btn_f("results_f"),
        bsTooltip("results_b",HTML("Return to <b>Run Analysis</b>")),
        bsTooltip("results_f",HTML("Proceed to <b>Enrichment Network</b>")))
    })
    #the two observeEvent function that control the two buttons
    observeEvent(input$results_b,{
        updateTabItems(session, "tabs", "gsea")
    })
    observeEvent(input$results_f,{
        updateTabItems(session, "tabs", "network")
    })

    # output$results_last_tab <- renderUI({
    #     div(actionBttn(inputId="results_last_tab",
    #                    icon = icon("unite"),
    #                    style = "simple",
    #                    size = "lg"
    #                    # color = ""
    #                    #status = "default", width = "250px",right=T,
    #     ),
    #     bsTooltip("results_last_tab","Click to go to \"Run Analysis\""))
    #
    # })
# # UI manhattan tables & words ---------------
#     output$ui_manhattan_table <- renderUI({
#         fluidRow(
#             tabBox(
#                 width = 12, title = "Significant Enrichment",
#                 tabPanel(
#                     "Table summary",
#                     uiOutput("ui_gsea_toggle"),
#                     br(),
#                     fluidRow(
#                         column(
#                             width = 6,
#                             uiOutput("ui_tables")
#                         ),
#                         column(
#                             width = 6,
#                             uiOutput("plot_words")
#                         )
#                     )
#                 )
#
#             )
#         )
#     })
# 


# Create a function for the go up buttons in kegg,reactome and wp plots
    scroll_up_button <- function(){
        div(
            absolutePanel(
                actionBttn(
                    inputId = "up_button", label=NULL, 
                    icon = icon("angle-double-up"), style="material-circle", color="primary", size="md"
                ),
                tags$script(HTML(
                    "document.getElementById('up_button').onclick= function(){
                    document.getElementById('plot_choice_box').scrollIntoView()
                };"
                )),
                right = 20,
                top = 4
            )
               
            )
    }
    
observeEvent(input$abb_bar_n, {
    check_numericInput("abb_bar_n", 40)
})
observeEvent(input$n_word, {
    check_numericInput("n_word", 10)
})
observeEvent(input$n_up_bar, {
    check_numericInput("n_up_bar", 10)
})
observeEvent(input$n_down_bar, {
    check_numericInput("n_down_bar", 10)
})
observeEvent(input$abb_bubble_n, {
    check_numericInput("abb_bubble_n", 40)
})
observeEvent(input$n_up_bubble, {
    check_numericInput("n_up_bubble", 10)
})
observeEvent(input$n_down_bubble, {
    check_numericInput("n_down_bubble", 10)
})

