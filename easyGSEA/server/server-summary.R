#=============================================================# 
######                 ENRICHMENT SUMMARY              ########
#=============================================================#
# -------- P or P.adj thresholds: UI -----------
output$summary_cutoffs <- renderUI({
    dropdown(
        style = "material-circle", icon = icon("gear"),align = "left",
        status = "default", width = "850px",
        right=T,
        animate = animateOptions(
            enter = "slideInRight",
            exit = "fadeOutRight", duration = 0.5
        ),
        fluidRow(
            column(
                width = 4,
                sliderTextInput("cutoff_sum_p",
                                label = "Adjust P threshold:",
                                choices= cutoff_slider,
                                selected=rv$vis_p, grid=T, force_edges=T)
            ),
            column(
                width = 4,
                sliderTextInput("cutoff_sum_q",
                                label = "Adjust P.adj threshold:",
                                choices= cutoff_slider,
                                selected=rv$vis_q, grid=T, force_edges=T)
            ),
            column(
                width = 3, offset = 1,
                br(),
                bsButton(
                    "sum_apply",
                    tags$b("Apply!"),
                    style = "danger",
                    size = "large"
                )
            )
        )
    )
})

# -------- P or P.adj thresholds: confirm -----------
observeEvent(input$sum_apply,{
    rv$vis_q = input$cutoff_sum_q
    rv$vis_p = input$cutoff_sum_p
})

# -------- GSEA run up/down toggle button ---------
output$ui_gsea_toggle <- renderUI({
    req(rv$run_mode=="gsea")
    req(rv$run == "success")
    # req(input$cutoff_sum_p)
    
    fluidRow(
        # column(
        #     width = 2,
        #     h4(tags$b("Direction of change"))
        # ),
        column(
            width = 10,
            radioGroupButtons(
                "tables_switch",
                NULL,
                choices = list("Upregulation"="up","Downregulation"="down"),
                selected = rv$tables_switch,
                size = "lg",
                checkIcon = list(
                    yes = icon("check-square"),
                    no = icon("square-o")
                ),
                status = "primary"
            )
            # switchInput(
            #     inputId = "tables_switch",
            #     value = FALSE,
            #     onLabel = "Up",
            #     offLabel = "Down",
            #     onStatus = "danger",
            #     offStatus = "primary",
            #     width = "100%"
            # )
        )
    )
})

# dynamically render manhattan tables & word bars -----------------
observe({
    req(rv$run == "success")
    req(rv$vis_p)
    req(rv$vis_q)

    df = filter_df_mh()
    
    # if gsea, further filter by direction of change
    if(rv$run_mode == "gsea"){
        req(input$tables_switch)
        
        rv$tables_switch = input$tables_switch
        
        # up or down
        direction <- rv$tables_switch
        
        # filter by cutoff
        if(direction == "up"){
            df = df %>% dplyr::filter(ES > 0)
            
        }else if(direction == "down"){
            df = df %>% dplyr::filter(ES < 0)
            
        }
    }

    # feedbacks on no significant enrichment
    no_sig = function(){
        if(rv$run_mode == "gsea"){
            HTML("No significant ",direction,"regulation found at pval < ",rv$vis_p," & q < ", rv$vis_q,
                 ". Please adjust thresholds by clicking the top-right gear button.")
        }else{
            HTML("No significant regulation found at pval < ",rv$vis_p," & q < ", rv$vis_q,
                 ". Please adjust thresholds by clicking the top-right gear button.")
        }
    }
    
    if(nrow(df)<1){
        output$ui_tables <- renderUI({
            no_sig()
        })
        
        output$plot_words <- renderUI({
            no_sig()
        })
    }else{
        
        withProgress(message = "Generating summary stats ...", value = 1,{
            
            # get db categories
            cats = unique(df$db)
            max_table = length(cats)
            
            # read in table content
            lst <- list()
            lst_words <- list()
            for (i in 1:max_table) {
                # get df == db
                data <- df %>% 
                    dplyr::filter(db == cats[i]) %>%
                    dplyr::select(-db) %>%
                    dplyr::arrange(padj)
                
                # save table data into lst
                lst[[i]] <- data %>%
                    mutate_if(is.numeric, function(x) round(x, digits=3))
                
                # create data for word freq count plots
                data <- data %>%
                    dplyr::mutate(linenumber = row_number(),text = pathway) %>%
                    dplyr::select(text,linenumber)
                
                data$text <- lapply(data$text,function(x) strsplit(x,"%")[[1]][1]) %>%
                    lapply(.,function(x) regmatches(x, regexpr("_", x), invert = TRUE)[[1]][2]) %>%
                    lapply(., function(x) gsub("_"," ",x)) %>%
                    unlist(.)
                
                # tidy and count data
                
                data <- data %>%
                    unnest_tokens(word, text) %>%
                    dplyr::anti_join(stop_words) %>%
                    dplyr::anti_join(useless_words) %>%
                    dplyr::filter(is.na(as.numeric(word))) %>%
                    dplyr::count(word,sort=TRUE)
                
                data <- data %>%
                    dplyr::mutate(total = sum(n)) %>%
                    dplyr::mutate(freq = n/total) %>%
                    dplyr::arrange(desc(freq)) %>%
                    dplyr::select(-total)
                
                lst_words[[i]] <- data
            }
            
            # UI tables
            output$ui_tables <- renderUI({
                plot_output_list <- lapply(1:max_table, function(i) {
                    tablename <- paste0("tablename", i)
                    column(width = 12,
                        tags$br(),
                        dataTableOutput(tablename,height = "300px")
                    )
                })
                do.call(tagList, plot_output_list)
            })
            
            # render tables' contents
            for (i in 1:max_table) {
                local({
                    my_i <- i
                    tablename <- paste0("tablename", my_i)
                    output[[tablename]] <- DT::renderDataTable({
                        DT::datatable(lst[[my_i]],
                                      extensions=c('Scroller','Buttons'),
                                      options = list(
                                          dom = 'Bfrtip',
                                          buttons = c('copy', 'pdf', 'print','csv', 'excel'),
                                          scrollY = "160px",
                                          scroller = TRUE,
                                          scrollX=TRUE           
                                      )
                        )
                    })
                })
            }
            
            # word frequency bar plots ----------
            output$plot_words <- renderUI({
                plot_output_list <- lapply(1:max_table, function(i) {
                    barname <- paste0("barname", i)
                    column(width = 12,
                        div(
                            style="overflow-y:scroll; overflow-x:scroll", #max-height:600px;
                            plotlyOutput(barname,height = "300px",width = "100%"),
                        ),
                        tags$br()
                    )
                })
                do.call(tagList, plot_output_list)
            })
            
            # render word count bar plots
            colors = brewer.pal(n = max_table, name = "Set2")
            for (i in 1:max_table) {
                local({
                    my_i <- i
                    barname <- paste0("barname", my_i)
                    output[[barname]] <- renderPlotly({
                        tidy_data = lst_words[[my_i]] %>%
                            mutate(word = factor(word, levels = rev(unique(word)))) %>%
                            top_n(15)
                            # head(.,n=15)
                        
                        
                        # hover text
                        text = lapply(tidy_data$word, function(x){
                            x = as.character(droplevels(x))
                            a = lst[[my_i]] %>%
                                dplyr::filter(str_detect(pathway,regex(x, ignore_case = TRUE))) %>%
                                dplyr::select(pathway) %>%
                                unlist(.) %>%
                                unname(.)
                            if(length(a)>14){a = c(a[1:14],"... ...")}
                            a = paste(a,collapse = "\n")
                            return(a)
                        })
                        
                        text = unlist(text)
                        
                        # # y axis label
                        # if(rv$run_mode == "glist"){
                        #     y_label = paste0("Word frequency (",names(rv$dbs)[rv$dbs==cats[my_i]],")")
                        # }else if(rv$run_mode == "gsea"){
                        #     if(direction == "up"){
                        #         y_label = paste0("Word frequency for upregulations (",names(rv$dbs)[rv$dbs==cats[my_i]],")")
                        #     }else if(direction == "down"){
                        #         y_label = paste0("Word frequency for downregulations (",names(rv$dbs)[rv$dbs==cats[my_i]],")")
                        #     }
                        # }
                        y_label = names(rv$dbs)[rv$dbs==cats[my_i]]
                        
                        p <- tidy_data %>%
                            ggplot(aes(word, n, text=text)) +
                            geom_col(show.legend = FALSE, fill = colors[my_i]) +
                            labs(x = NULL, y = NULL, title = y_label) +
                            coord_flip() +
                            scale_x_reordered() +
                            theme(
                                plot.title = element_text(size = 10,face = "bold",vjust=0) #hjust = 0.5
                            )
                        
                        # adjust plot height
                        lth = nrow(tidy_data) * 18 + 50
                        if(lth<300){lth=300}
                        
                        ggplotly(p, height = lth,
                                 # margin=dict(
                                 #     l=250,
                                 #     r=0,
                                 #     b=0,
                                 #     t=50,
                                 #     pad=0
                                 # ),
                                 tooltip=c("word","n","text"))
                    })
                })
            }
        })
        
        
    }
})

# ------------- FUNCTIONS: text mining -------------

filter_df_mh <- function(){
    # retrieve data
    df = rv$fgseagg %>% dplyr::filter(!(is.na(pval)))
    p = rv$vis_p
    q = rv$vis_q
    
    # filter by cutoff
    df = df %>% dplyr::filter(pval<p & padj<q)
    
    return(df)
}
