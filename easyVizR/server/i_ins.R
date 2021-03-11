####================= MULTIPLE - INTERSECT =====================####
palette <- c("aquamarine","blue","darkgrey","darksalmon","red","lightgrey","lightblue","lightgreen","lightyellow","white","yellow") # this is the default base palette for venns
n_wc_ignore_help_txt <- "Common expressions:<br><b>%.*$</b> - removes trailing identifiers from easyGSEA<br><b>[[:digit:]]</b> - removes digits"


#======================================================================#
####                3.2 INTERSECTION TABLE                          ####
#======================================================================#

####-------------------- Intersection selection ------------------------####

# generates dynamic ui for selection
observe({
  
  req(nrow(rv$df_n)>0)
  req_filter_ns("nic",rv)
  
  criteria <- rv$ins_criteria
  criteria <- as.character(criteria)
  criteria[is.na(criteria)] <- "Ignore"
  criteria <- gsub("FALSE","False", criteria)
  criteria <- gsub("TRUE", "True",criteria)
  print(criteria)
  for (i in 1:length(rv$nx_n)){
    rv$s[[i]] <- div(style="display: inline-block;vertical-align:top; width: 280px;",
                     radioGroupButtons(inputId = paste0("ins_",i),
                                       label = rv$nx_n[[i]], size="s",
                                       choices = c("True", "False", "Ignore"),
                                       selected =criteria[[i]],
                                       status = "info"),
                     
                     radioTooltip(id = paste0("ins_",i), choice = "True", 
                                  title = paste0("Included in the Venn circle.","<br>(",summarize_filter("nic",rv,rv$nx_n[[i]],T,include_name=F)," in this dataset)"), 
                                  placement = "bottom"),
                     radioTooltip(id = paste0("ins_",i), choice = "False", 
                                  title = paste0("Excluded from the Venn circle.","<br>(",summarize_filter("nic",rv,rv$nx_n[[i]],F,include_name=F)," in this dataset)"), 
                                  placement = "bottom"),
                     radioTooltip(id = paste0("ins_",i), choice = "Ignore", 
                                  title = "Filters ignored", 
                                  placement = "bottom"),

                     )
  }
  rv$s_button[[1]] <- div(style="display: inline-block;margin-top: 1.55em; width: 280px;",
                     actionButton("ins_applytorv", "Save selection", class = "btn-warning")
                     )
    
  
})

# Draws radiogroupbuttons ui for intersection selection
output$ui_intersections <- renderUI({
  req(nrow(rv$df_n)>0)
  append(x = rv$s, value=rv$s_button, length(rv$s))
    
})

# updates criteria
observeEvent(input$ins_applytorv, {
  req(rv$df_n)
  req(length(rv$s)==length(rv$nx_i))
  
  # turn those T/F inputs into a true/ false vector
  criteria <- vector(mode = "list", length = length(rv$v))
  for (i in 1:length(rv$s)){
    criteria[[i]] <- input[[paste0("ins_", i)]]
  }
  criteria <- unlist(criteria)
  criteria <- gsub("False",FALSE, criteria)
  criteria <- gsub("True",TRUE, criteria)
  criteria <- gsub("Ignore",NA, criteria)
  criteria <- as.logical(criteria)
  
  # name the criteria vector
  if (length(criteria) == length(rv$nx_n)){
    names(criteria) <- rv$nx_n
  }
  # print(criteria)
  rv$ins_criteria <- criteria
  refresh_vis_ui()
})


####-------------------- Active Filters summary ------------------------####

# summarizes verbally what is in the selected intersection
output$intersection_summary <- renderUI({
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria, rv$nx_n
  ), check_len=T)
  
  # save the RVs for demo session
  if(!is.null(rv$demo_save) && rv$demo_save == "yes"){
    variable_list <- c("all_char_stats", "batch_failed", "batch_files", "columnCount", "detected_dbs",
                       "df_n", "FileDf", "folder_upload_state", "gg", "gls_text",
                       "gls_ui", "heatmap_i", "heatmap_sortby", "hm_numeric_stats",
                       "ins_criteria", "ins_venn_palette", "ll", "n_3ds_status", "n_css_highlights",
                       "n_igl", "n_ins_view", "n_sharedcols", "n_sharedrows", "n_to_plot", "n_ui_showpanel",
                       "n_venn_status", "nic", "nw_char_stats", "nx_i", "nx_n", "opt_easygsea_filter_db",
                       "s", "tt", "upload_batch_colscheme", "upload_batch_columns", "upload_batch_sharedcols",
                       "upload_columns", "v", "n_ins_namelen")
    for(i in seq_along(variable_list)){
      saveRDS(rv[[variable_list[i]]], file = paste0("rvs/", variable_list[i], ".rds"))
    }
  }
  
  desc <-list()
  criteria <- rv$ins_criteria
  req_filter_ns("nic", rv)
  
  # generate a description for every dataset and put into list
  for (i in 1:length(rv$nx_n)){
    # only append a description when it's not on ignore
    adddesc <- summarize_filter("nic", rv, rv$nx_n[[i]], criteria[[rv$nx_n[[i]]]])
    if (is.na(adddesc)==F){
      desc <- c(desc, adddesc)
    }
  }
  
  # make into html unordered list
  if (length(desc)>0){
    text <- paste0("<ul>", paste(paste0("<li>",desc,"</li>"), collapse=""), "</ul>")
    HTML(text)
  } else {
    HTML("No filters are active.")
  }
  
})

####-------------------- Intersection table ------------------------####


# show intersection table
output$n_ins_tbl <- DT::renderDataTable({
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria, rv$nx_n,
    n_ins_df(),
    rv$n_ins_namelen
  ), check_len=T)

  df <- n_ins_df()
  # print(head(n_ins_df()))
  
  rv$df_ins_fullcols <- colnames(df)
  
  # to abbreviate the long column names...take first 5 letters
  char_limit <- 56 / length(colnames(df))
  # print(char_limit)
  colnames(df) <- abbreviate_vector(vec=colnames(df), 
                                    char_limit=char_limit)
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  
  df
  
  
}
, plugins = "ellipsis",
options = list(scrollX=TRUE, 
               columnDefs = list(
                 list(
                   targets = 1,
                   render = JS(
                     sprintf("$.fn.dataTable.render.ellipsis( %s, true )", toString(rv$n_ins_namelen))
                     )
                 ),
                 list(
                   targets = "_all",
                   render = JS("$.fn.dataTable.render.ellipsis( 6, true )")
                 )
               ),
               headerCallback= JS("function(thead, data, start, end, display){",
                                  sprintf("  var tooltips = [%s];", toString(paste0("'", rv$df_ins_fullcols, "'"))),
                                  "  for(var i = 1; i <= tooltips.length; i++){",
                                  "    $('th:eq('+i+')',thead).attr('title', tooltips[i-1]);",
                                  "  }",
                                  "}"))
)


# download intersection table as csv
output$download_ins_df <- downloadHandler( # needs to be exactly the same as table render
  filename = function() {
    paste("intersection", "-", "multiple", "-", Sys.Date(), ".csv", sep="")},
  content = function(file) {
    write.csv(n_ins_df(), file, 
              row.names = F, quote=TRUE)})

# download filtered gene list as txt
output$download_ins_gl <- downloadHandler(
  filename = function() {
    paste("names", "-", "multiple", "-", Sys.Date(), ".txt", sep="")},
  content = function(file) {
    ins <- n_ins_full()$Name
    fwrite(list(ins), file, sep=",", 
           row.names = F, quote=F)
  }
)




#======================================================================#
####                3.1B UPSET PLOT                          ####
#======================================================================#


n_upset_plt <- reactive({
  
  gls <- n_ins_gls()
  
  draw_upsetR_with_ins(fromList(gls), rv$ins_criteria, 
                       show_ins=rv$n_upset_show_ins, color=rv$n_upset_c1, 
                       empty_intersections = rv$n_upset_showempty,
                       order_by = rv$n_upset_sortby,
                       text_scale=c(1.3, 2, 1.3, 1, 1.1, 2)
  )
  
})


# gl ver (uses the shared reactive)
output$df_n_upset <- renderPlot({
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria,
    rv$n_upset_showempty, rv$n_upset_sortby,
    rv$n_upset_c1, rv$n_upset_show_ins
  ), check_len=T)
  
  validate( 
    need(min(lengths(n_ins_gls()))>0, gls_has_empty_msg),
    need(nrow(rv$df_n)>0, df_n_empty_msg)
  )
  
  n_upset_plt()
})

output$n_upset_dl <- downloadHandler(
  filename = function() { paste("upset-multiple-",Sys.Date(), '.pdf', sep='') },
  content = function(file) {
    pdf(file)
    print(n_upset_plt())
    dev.off()
  }
)



#======================================================================#
####                3.1A.2 AREA PROPORTIONAL VENN                          ####
#======================================================================#

output$n_npvenn_dl <- downloadHandler(
  filename = function() { paste("venn1-multiple-",Sys.Date(), '.png', sep='') },
  content = function(file) {
    req(is.null(n_npvenn_plt())==F)
    ggsave(file, plot = n_npvenn_plt(), device = "png")
  }
)

output$n_venn_dl <- downloadHandler(
  filename = function() { paste("venn2-multiple-",Sys.Date(), '.png', sep='') },
  content = function(file) {
    req(is.null(n_venn_plt())==F)
    ggsave(file, plot = n_venn_plt(), device = "png")
  }
)






# gl ver (uses the shared reactive)
n_venn_plt <- reactive({
  
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria, rv$nx_n,
    rv$n_venn_label, rv$n_venn_show_ins, rv$ins_venn_c1, rv$ins_venn_palette
  ), check_len=T)
  
  validate( 
    need(min(lengths(n_ins_gls()))>0, gls_has_empty_msg),
    need(nrow(rv$df_n)>0, df_n_empty_msg)
  )
  
  
  draw_eulerr_with_ins(n_ins_gls(), rv$ins_criteria, 
                       print_mode=rv$n_venn_label, 
                       show_ins=rv$n_venn_show_ins, ins_color=rv$ins_venn_c1, base_color=rv$ins_venn_palette,
                       adjust_labels=T)
  
})

output$df_n_venn <- renderPlot({
  req(rv$n_venn_type=="Area-proportional")
  
  n_venn_plt()
})



#======================================================================#
####                3.1A.1 NON AREA PROPORTIONAL VENN                          ####
#======================================================================#

n_npvenn_plt <- reactive({
  
  req_vars(c(
    rv$df_n, n_ins_gls(), rv$ins_criteria, rv$nx_n,
    rv$n_venn_label, rv$n_venn_show_ins, rv$ins_venn_c1, rv$ins_venn_palette
  ), check_len=T)
  
  validate( 
    need(min(lengths(n_ins_gls()))>0, gls_has_empty_msg),
    need(nrow(rv$df_n)>0, df_n_empty_msg)
  )
  
  grid.newpage()
  
  # venn1 <- venn.diagram(gls, filename = NULL,
  #                       lwd = 1, # border width
  #                       fontfamily = "sans",
  #                       cat.fontface = "bold",
  #                       cex = 1, # catname size
  #                       # cat.pos = rep(180,len), # catname position
  #                       cat.cex = 1, # areaname size
  #                       cat.fontfamily = "sans",
  #                       fill = palette, # area fill
  #                       alpha = 0.5, # area alpha
  #                       ext.text=T, # draw label outside in case no space
  #                       ext.line.lty = "dotted", # pattern of extline
  #                       sigdigs = 2,
  #                       print.mode = gsub("counts","raw", rv$n_venn_label)
  # )
  
  venn1 <- draw_venn_with_ins(n_ins_gls(), rv$ins_criteria, rv$nx_n,
                              gsub("counts","raw", rv$n_venn_label),
                              lb_limit=20,
                              show_ins=rv$n_venn_show_ins, ins_color=rv$ins_venn_c1, base_color=rv$ins_venn_palette
                              )
  venn1
})

output$df_n_npvenn <- renderPlot({

  grid.draw(n_npvenn_plt())

  
})



# ----------------------- exclusion report

output$venn_exclusion_report <- renderUI({
  req_vars(c(n_ins_gls(),rv$ins_venn_c1,rv$ins_criteria))
  req_df(rv$df_n)
  
  excluded <- nrow(rv$df_n)-length(Reduce(union, n_ins_gls()))
  
  if (T %in% rv$ins_criteria ==F & rv$n_venn_show_ins==T){  # only highlight if user selected these excluded genes
    rgbhl <- paste(as.vector(col2rgb(rv$ins_venn_c1)), collapse = ",")
    options <- paste0("color:",rv$ins_venn_c1,";text-shadow: 0px 0px 5px rgba(",rgbhl,", 0.3)") # apply a color+dropshadow effect
  } else {options="color:black"}
  HTML(paste0("<span style='",options,";'>Not in any circle: ", excluded,"</span>"))
})


#------------------- Conditionally show either Venn -------------------

output$n_venn_ui <- renderUI({
  req(is.null(rv$n_venn_type)==F)
  if (rv$n_venn_type=="Basic"){
    plotOutput("df_n_npvenn", width="100%")
  } else if (rv$n_venn_type=="Area-proportional"){
    plotOutput("df_n_venn", width="100%")
  }
})



#======================================================================#
####                3.2.1 WORDCLOUD                          ####
#======================================================================#

# compute the frequency table
n_ins_wc_df <- reactive({
  df <- generate_wc_freq_table(
    vec=n_ins_full()$Name,
    sep=input$n_ins_wc_sep,
    ignored=input$n_ins_Wc_ignore
  )
  
  df
})

# show the frequency table
output$n_ins_wc_df <- DT::renderDataTable({
  req(nrow(n_ins_wc_df())>0)
  n_ins_wc_df()[order(n_ins_wc_df()$Freq, decreasing=T),]
}
, plugins="ellipsis",
options=list(scrollX=T, scrollY=T, dom= 'tp',
             pageLength = 10
),
rownames= FALSE
)

# draw wordcloud plot
n_ins_wc_plt <- reactive({
  
  validate(
    need(nrow(n_ins_wc_df())>0, wc_no_word), # if no words in table
    need(max(n_ins_wc_df()$Freq)>1, wc_no_repeated_word) # if no repeated words are found
  ) 
  df <- n_ins_wc_df()
  generate_wc_plot(df)
  
})

# show wordcloud plot
output$n_ins_wc <- renderPlot({
  req(length(n_ins_full()$Name)>0)
  
  n_ins_wc_plt()
})


#======================================================================#
####                     intersection UI                            ####
#======================================================================#

#----------------- ins table --------------------

output$ins_table_panel <- renderUI({
  
  div(
    
    div(id="n2_4",
        
        wellPanel(
          HTML(paste0(
            "Combine options below to select an intersection of interest:",
            add_help("ins_help", style="margin-left: 5px;margin-bottom:10px;"))
          ),
          bsTooltip("ins_help", 
                    "Hover over the options to see the corresponding filters.", 
                    placement = "top"),
          uiOutput("ui_intersections"),
          
          HTML(paste0(
            "Filtering conditions for current intersection:",
            add_help("active_filters_help", style="margin-left: 5px;"))
          ),
          bsTooltip("active_filters_help", 
                    "Filtering conditions for this intersection are displayed below", 
                    placement = "top"),
          uiOutput("intersection_summary")
        ),
    ),
    
    
    
    dataTableOutput("n_ins_tbl"),
    
    
    div(id = "intersection_table_setting_button",
      style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          radioGroupButtons(
            inputId = "n_ins_view",
            label = "Choose view:",
            choices = c("Full", "Minimized", "T/F Matrix"),
            selected= rv$n_ins_view, direction="vertical"
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE
        ),
        radioTooltip(id = "n_ins_view", choice = "Full", 
                     title = "Show all columns for datasets", 
                     placement = "right"),
        radioTooltip(id = "n_ins_view", choice = "Minimized", 
                     title = stat_replace1("Only show the essential columns (Name, Stat, PValue, FDR)", rv$nx_n), 
                     placement = "right"),
        radioTooltip(id = "n_ins_view", choice = "T/F Matrix", 
                     title = "Show true/false value depending on if the corresponding filters conditions are met", 
                     placement = "right"),
        
    ),
    div(id = "intersection_table_color_button",
      style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          sliderInput("n_ins_namelen",
                      "Max string length for Name column:",
                      min = 25,
                      max = 80,
                      value = rv$n_ins_namelen),
          
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=300
        ),
        
    ),
    
    div(style = "position: absolute; left: 7em; bottom: 1em", id="n2_4b",
        dropdown(inputId="n_wc_dropdown",
                 column(12, h4("Text enrichment wordcloud (for Name column)")),
                 column(8,
                        column(2, textInput("n_ins_wc_sep", "Separator:", value="_")),
                        column(10, textInput("n_ins_Wc_ignore", 
                                             HTML(paste0(
                                               "Ignore strings: (separated by spaces)",
                                               add_help("n_wc_ignore_help", style="margin-left: 5px;"))
                                             ), 
                                             value="and or of GO KEGG WP RA C2"),
                               bsTooltip("n_wc_ignore_help", 
                                         n_wc_ignore_help_txt, 
                                         placement = "top"),
                               ),
                        
                        plotOutput("n_ins_wc"),
                 ),
                 column(4,
                        dataTableOutput("n_ins_wc_df"),
                 ),
                 
                 size = "xs",
                 icon = icon("cloud", class = "opt", lib="glyphicon"),
                 up = TRUE, width=800
        )
        
    ),
    # bsTooltip("n_wc_dropdown", "Text enrichment wordcloud (for gene set-type terms)", placement = "top"),
    
    div(id = "intersection_table_download_button",
      style = "position: absolute; left: 10em; bottom: 1em",
        dropdown(
          downloadButton("download_ins_df", "Download current table"),
          downloadButton("download_ins_gl", "Download gene list"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
    
  )
})


#----------------- ins vis  --------------------

#output$n_ui_intersect <- renderUI({
  # req(rv$n_ui_showpanel == "Intersection")

#})

#----------------- venn --------------------
output$n_venn_ins_hl_opt <- renderUI({
  req_vars(rv$n_venn_show_ins)
  req(rv$n_venn_show_ins==T)
  div(
    selectInput("ins_venn_c1", 
                HTML(paste0(
                  "<b>Intersection highlight color:</b>",
                  add_help("ins_venn_c1_help", style="margin-left: 5px;"))
                ),
                choices = default_colors,
                selected=rv$ins_venn_c1
    ),
    bsTooltip("ins_venn_c1_help", 
              "Highlight color for selected intersection (corresponds to table below)", 
              placement = "top")
  )
})
#Newly added graph color selector
output$n_venn_ins_palette <- renderUI({
  req_vars(rv$n_venn_show_ins)
  #req(rv$n_venn_show_ins==T)
  div(
    selectInput("ins_venn_palette", 
                HTML(paste0(
                  "<b>color(s) for parts not in the intersection:</b>",
                  add_help("ins_venn_palette_help", style="margin-left: 5px;"))
                ),
                choices = default_colors,
                selected=rv$ins_venn_palette,
                multiple = TRUE
    ),
    bsTooltip("ins_venn_palette_help", 
              "This is a multiple choices color picker for non-intersection parts of the graph", 
              placement = "top")
  )
})

output$venn_dropdowns <- renderUI({
  div(
    div(id = "venn_diagram_setting_button",
      style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          checkboxGroupInput(
            inputId = "n_venn_label",
            label= "Show in label:",
            choices = c("Counts"="counts", "Percent"="percent"),
            selected=rv$n_venn_label,
            inline=T, width="250px"
          ),
          radioGroupButtons(
            inputId = "n_venn_type",
            label = "Venn type:",
            choices = c("Basic","Area-proportional"),
            selected = rv$n_venn_type, direction="horizontal"
          ),
          radioGroupButtons(
            inputId = "n_venn_show_ins",
            label = HTML(paste0(
              "<b>Highlight selected intersection?</b>",
              add_help("n_venn_show_ins_help", style="margin-left: 5px;"))
            ),
            choices = c("Yes"=T,"No"=F),
            selected = rv$n_venn_show_ins, direction="horizontal"
          ),
          bsTooltip("n_venn_show_ins_help", 
                    "Whether to highlight selected intersection (corresponds to table below)", 
                    placement = "top"),
          
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE
        )
    ),
    #div(id = "venn_gear_dropdowns_anchor"), #,style = "position: absolute; left: 1em; bottom: 1em"),
    div(id = "venn_diagram_color_button",
      style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          uiOutput("n_venn_ins_hl_opt"),
          uiOutput("n_venn_ins_palette"),
          size = "xs",
          icon = icon("palette", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    #div(id = "venn_color_dropdowns_anchor"),#,style = "position: absolute; left: 4em; bottom: 1em"),
    div(id = "venn_diagram_download_button",
      style = "position: absolute; left: 7em; bottom: 1em",
        dropdown(
          downloadButton("n_npvenn_dl", "Download basic"),
          downloadButton("n_venn_dl", "Download area-proportional"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
    )
  )
  #div(id = "venn_download_dropdowns_anchor"), #,style = "position: absolute; left: 7em; bottom: 1em"),
})


ins_venn_panel <- reactive({
  box(title = span( strong("3.1a."),icon("chart-area"), "Venn Diagram"), status = "primary", solidHeader = F, width=6,
      
      uiOutput("n_venn_ui"),
      
      div(id = "venn_dropdowns_anchor"),
            
      div(style = "position: absolute; right: 1em; bottom: 1em", 
          uiOutput("venn_exclusion_report")
          
      )
      
      
  )
})



#----------------- upset --------------------
output$upset_dropdowns <- renderUI({
  div(id = "upset_diagram_buttons",
  div(id = "upset_diagram_setting_button",
      style = "position: absolute; left: 1em; bottom: 1em",
      dropdown(
        selectInput(
          inputId = "n_upset_sortby",
          label = "Order by:",
          choices = c("Frequency"="freq", "Degree"="degree"),
          selected = rv$n_upset_sortby),
        materialSwitch(
          inputId = "n_upset_showempty", label = "Show empty intersections?", status="primary",
          value = rv$n_upset_showempty
        ),
        radioGroupButtons(
          inputId = "n_upset_show_ins",
          label = HTML(paste0(
            "<b>Highlight selected intersection?</b>",
            add_help("n_upset_show_ins_help", style="margin-left: 5px;"))
          ),
          choices = c("Yes"=T,"No"=F),
          selected = rv$n_upset_show_ins, direction="horizontal"
        ),
        bsTooltip("n_upset_show_ins_help", 
                  "Whether to highlight selected intersection (corresponds to table below)", 
                  placement = "top"),
        
        size = "xs",
        icon = icon("gear", class = "opt"),
        up = TRUE, width=300
      )
      
  ),
  div(id = "upset_diagram_color_button",
    style = "position: absolute; left: 4em; bottom: 1em",
      dropdown(
        selectInput("n_upset_c1", 
                    HTML(paste0(
                      "<b>Intersection highlight color:</b>",
                      add_help("n_upset_c1_help", style="margin-left: 5px;"))
                    ),
                    choices = default_colors,
                    selected=rv$n_upset_c1
        ),
        bsTooltip("n_upset_c1_help", 
                  "Highlight color for selected intersection (corresponds to table below)", 
                  placement = "top"),
        
        size = "xs",
        icon = icon("palette", class = "opt"),
        up = TRUE, width=300
      )
      
  ),
  
  div(id = "upset_diagram_download_button",
    style = "position: absolute; left: 7em; bottom: 1em",
      dropdown(
        downloadButton("n_upset_dl", "Download plot"),
        
        size = "xs",
        icon = icon("download", class = "opt"),
        up = TRUE, width=300
      )
      
  )
  
  )
})


ins_upset_panel <- reactive({
  
  box(
    title = span( strong("3.1b."),icon("chart-area"), "UpSet Plot"), status = "primary", solidHeader = F, width=6,
    
    plotOutput("df_n_upset", width = "100%"),
    
    div(id = "upset_dropdowns_anchor")

    
  )
})

output$n_venn_placeholder <- renderUI({
  box(
    title = span( strong("3.1a."),icon("chart-area"), "Venn Diagram"), status = "warning", solidHeader = F, width=6,
    paste0("Venn diagram is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})
output$n_upset_placeholder <- renderUI({
  box(
    title = span( strong("3.1b."),icon("chart-area"), "UpSet Plot"), status = "warning", solidHeader = F, width=6,
    paste0("UpSet plot is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})




#======================== ASSEMBLE INTO A PAGE ========================#

output$ins_main_panels <- renderUI({
  if (is.null(rv$df_n)==T){
    box(
      title = span( icon("exclamation"), "Notification"), status = "warning", width=8,
      "No data selected."
    )
  } else {
    div(
      fluidRow(style = "position:relative;z-index:9999",
        div(style="height: 3.5em;",
            column(6,
                   HTML("<span style='font-size: 160%;margin-left: 0.5em;'>Select Intersection</span>"),
                   
            ),
            column(6,align= "right",
                   div(
                       id="ins_filters_here"),
                   
            )
        )
        
      ),
      fluidRow(
        div(
          column(
            width = 12,
            div(id="n0_4", style="height:400px",
                #uiOutput("n_ui_intersect"),
                
                div(id = "n_ui_intersect",
                    
                    #----------------- venn --------------------
                    
                    conditionalPanel("output.n_venn_status == 'ok'",
                                     ins_venn_panel(),
                    ),
                    
                    conditionalPanel("output.n_venn_status == 'no'",
                                     uiOutput("n_venn_placeholder")
                    ),
                    
                    
                    
                    #----------------- upset --------------------
                    conditionalPanel("output.n_venn_status == 'ok'",
                                     ins_upset_panel(),
                    ),
                    conditionalPanel("output.n_venn_status == 'no'",
                                     uiOutput("n_upset_placeholder")
                    ),
                    
                    
                    
                )
                #div(id="ins_graph_here"),
                
            )
            
          )
        )
      ),
      
      fluidRow(
        column(12,
               box(
                 width = 12, status = "primary",solidHeader = F,
                 title = span(strong("3.2."),icon("table"),"Intersection of Interest"),
                 div(id= "ins_pg_bottom"),
               )
               
        )
      )
    )
  }
})