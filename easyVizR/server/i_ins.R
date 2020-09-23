####================= MULTIPLE - INTERSECT =====================####


####-------------------- Intersection selection ------------------------####

# generates dynamic ui for selection
observe({
  req(nrow(rv$df_n)>0)
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
                                       status = "info"))
  }
  rv$s_button[[1]] <- div(style="display: inline-block;margin-top: 1.55em; width: 280px;",
                     actionButton("ins_applytorv", "Apply selection", class = "btn-warning")
                     )
    
  
})

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
})


####-------------------- Active Filters summary ------------------------####

# summarizes verbally what is in the selected intersection
output$intersection_summary <- renderUI({
  req(is.null(rv$ins_criteria)==F)
  req(length(rv$ins_criteria)>0)
  
  desc <-list()
  criteria <- rv$ins_criteria
  
  for (i in 1:length(rv$nx_n)){
    req(is.null(rv[[paste0("nic_p_",i)]])==F)
    req(is.null(rv[[paste0("nic_q_",i)]])==F)
    req(is.null(rv[[paste0("nic_Stat_",i)]])==F)
    req(is.null(rv[[paste0("nic_sign_",i)]])==F)
    
    name <- rv$nx_n[[i]]
    cur_p <- rv[[paste0("nic_p_",i)]]
    cur_q <- rv[[paste0("nic_q_",i)]]
    cur_Stat <- rv[[paste0("nic_Stat_",i)]]
    cur_sign <- rv[[paste0("nic_sign_",i)]]
    
    
    req(is.null(criteria[[name]])==F)
    if (is.na(criteria[[name]])==T){ # if NA
      stat_text=""
    } else if (criteria[[name]]==F){ # if FALSE
      if (cur_p<1){ # p cutoff is only meaningful if <1
        p_text <- paste0("p > ",cur_p)
      } else {p_text <- NA}
      if (cur_q<1){ # q cutoff is only meaningful if <1
        q_text <- paste0("q > ",cur_q)
      } else {q_text <- NA}
      
      if (cur_sign=="All"){ # if FALSE and ALL
        if (cur_Stat>0){ # |Stat| cutoff is only meaningful if >0
          stat_text <- paste0("|Stat| < ", cur_Stat)
        } else {stat_text <- NA} 
      } else if (cur_sign=="Positive"){ # if FALSE and POS
        stat_text <- paste0("Stat < ", cur_Stat)
      } else if (cur_sign=="Negative") { # if FALSE and NEG
        stat_text <- paste0("Stat >  ", cur_Stat)
      }
    } else if (criteria[[name]]==T){ # if TRUE
      if (cur_p<1){
        p_text <- paste0("p <= ",cur_p)
      } else {p_text <- NA}
      if (cur_q<1){
        q_text <- paste0("q <= ",cur_q)
      } else {q_text <- NA}
      
      if (cur_sign=="All"){ # if TRUE and ALL
        if (cur_Stat>0){ # |Stat| cutoff is only meaningful if >0
          stat_text <- paste0("|Stat| >= ", cur_Stat) 
        } else {stat_text <- NA} 
      } else if (cur_sign=="Positive"){ # if TRUE and POS
        stat_text <- paste0("Stat >= ", cur_Stat)
      } else if (cur_sign=="Negative") { # if TRUE and NEG
        stat_text <- paste0("Stat <=  ", cur_Stat)
      }
    } 
    
    if (is.na(criteria[[name]])==F){
      if (criteria[[name]]==T){
        cond_string <- paste(
          paste0("<strong>", na.omit(c(p_text, q_text, stat_text)), "</strong>")
          , collapse=" AND ")
        adddesc <- paste(
          cond_string,
          " in ", "<i>",name,"</i>",
          sep="")
      } else if (criteria[[name]]==F){
        cond_string <- paste(
          paste0("<strong>", na.omit(c(p_text, q_text, stat_text)), "</strong>")
          , collapse=" OR ")
        adddesc <- paste(
          cond_string,
          " in ", "<i>",name,"</i>",
          sep="")
      } 
      # only append a description when it's not on ignore
      desc <- c(desc, adddesc)
    }
  }
  # print(desc)
  if (length(desc)>0){
    text <- paste0("<ul>", paste(paste0("<li>",desc,"</li>"), collapse=""), "</ul>")
    HTML(text)
  } else {
    HTML("No filters are active.")
  }
  
})

# summarizes verbally the filters used to trim gene lists
output$filters_summary <- renderUI({
  
  desc <-list()
  
  for (i in 1:length(rv$nx_n)){
    req(rv[[paste0("nic_p_",i)]])
    req(rv[[paste0("nic_q_",i)]])
    req(rv[[paste0("nic_Stat_",i)]])
    req(rv[[paste0("nic_sign_",i)]])
    
    name <- rv$nx_n[[i]]
    cur_p <- rv[[paste0("nic_p_",i)]]
    cur_q <- rv[[paste0("nic_q_",i)]]
    cur_Stat <- rv[[paste0("nic_Stat_",i)]]
    cur_sign <- rv[[paste0("nic_sign_",i)]]
    if (cur_sign=="All"){
      cur_sign = "Positive and Negative"
    } else {
      cur_sign <- cur_sign
    }
    
    nterms <- length(n_ins_gls()[[i]]) # number of genes in genelist
    
    adddesc <- paste(name, ": ",
                     nterms, " ",
                     cur_sign, " entries with ",
                     "p <= ",cur_p, ", ", 
                     "FDR <= ", cur_q, ", ", 
                     "|Stat| >= ", cur_Stat,
                     sep="")
    desc <- c(desc, adddesc)
  }
  text <- paste(desc, collapse="<br>")
  
  
  HTML(text)
})




####-------------------- Intersection table ------------------------####

# show intersection preview table
output$n_ins_tbl <- DT::renderDataTable({
  req(is.null(rv$df_n)==F)
  req(length(rv$ins_criteria)>0)

  df <- n_ins_df()
  
  rv$df_ins_fullcols <- colnames(df)
  
  # to abbreviate the long column names...take first 5 letters
  char_limit <- 56 / length(colnames(df))
  # print(char_limit)
  colnames(df) <- sapply(names(df), function(x){
    if (nchar(x)>char_limit)
    {return (paste0(substr(x, start = 1, stop = char_limit),"..."))}
    else{return (x)}
  })
  
  # to round everything down to 3 decimals
  df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., 3))
  
  
  df
  
  
}
, plugins = "ellipsis",
options = list(scrollX=TRUE, 
               columnDefs = list(
                 list(
                   targets = 1,
                   render = JS("$.fn.dataTable.render.ellipsis( 17, true )")
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
    ins <- n_ins_fgl()
    fwrite(list(ins), file, sep=",", 
           row.names = F, quote=F)
  }
)




####-------------------- Upset plot ------------------------####

n_upset_plt <- reactive({
  gls <- n_ins_gls()
  
  names(gls) <- gsub(".csv","",names(gls))
  #c(intersection size title, intersection size tick labels, set size title, set size tick labels, set names, numbers above bars)
  textscale <- c(1.3, 2, 1.3, 1, 1.1, 2)
  if (rv$n_upset_showempty==F){
    upset <- upset(fromList(gls), order.by = rv$n_upset_sortby,
                   text.scale = textscale)
  }
  else if (rv$n_upset_showempty==T){
    upset <- upset(fromList(gls), empty.intersections = "on", order.by = rv$n_upset_sortby,
                   text.scale = textscale)
  }
  upset
})


# gl ver (uses the shared reactive)
output$df_n_upset <- renderPlot({
  req(rv$df_n)
  req(is.null(rv$n_upset_showempty)==F)
  req(is.null(rv$n_upset_sortby)==F)
  req(max(lengths(n_ins_gls()))>0)
  req(min(lengths(n_ins_gls()))>0)
  req(length(n_ins_gls())>1)
  
  
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




####-------------------- Venn plot ------------------------####

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


#------------------- eulerR area proportional venn
# gl ver (uses the shared reactive)
n_venn_plt <- reactive({
  gls <- n_ins_gls()
  names(gls) <- gsub(".csv","",names(gls))
  fit2 <- euler(gls)
  venn <- plot(fit2, quantities = list(type = rv$n_venn_label))
  
  venn
})

output$df_n_venn <- renderPlot({
  req(length(n_ins_gls())>0)
  req(max(lengths(n_ins_gls()))>0)
  req(rv$n_venn_type=="Area-proportional")
  
  n_venn_plt()
})


# #------------------- VennDiagram non area proportional venn
n_npvenn_plt <- reactive({
  req(length(n_ins_gls())>0)
  req(max(lengths(n_ins_gls()))>0)
  req(n_ins_gls())
  
  grid.newpage()
  gls <- n_ins_gls()
  names(gls) <- gsub(".csv","",names(gls))
  len <- length(gls)
  palette <- c("white","grey","darkgrey","lightgrey","black")
  palette <- palette[1:len]
  venn1 <- venn.diagram(gls, filename = NULL,
                        lwd = 1, # border width
                        fontfamily = "sans",
                        cat.fontface = "bold",
                        cex = 1, # catname size
                        # cat.pos = rep(180,len), # catname position
                        cat.cex = 1, # areaname size
                        cat.fontfamily = "sans",
                        fill = palette, # area fill
                        alpha = 0.5, # area alpha
                        ext.text=T, # draw label outside in case no space
                        ext.line.lty = "dotted", # pattern of extline
                        sigdigs = 2,
                        print.mode = gsub("counts","raw", rv$n_venn_label)
  )
  venn1
})

output$df_n_npvenn <- renderPlot({
  req(length(n_ins_gls())>0)
  req(max(lengths(n_ins_gls()))>0)
  req(rv$n_venn_type=="Basic")
  
  
  
  grid.draw(n_npvenn_plt())
  
  
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




####--------------------wordcloud for pathways------------------------####

map2color<-function(x,pal,limits=NULL){
  if(is.null(limits)) limits=range(x)
  pal[findInterval(x,seq(limits[1],limits[2],length.out=length(pal)+1), all.inside=TRUE)]
}

# compute the frequency table
n_ins_wc_df <- reactive({
  vc <- n_ins_fgl()
  sep <- input$n_ins_wc_sep
  words <- unlist(lapply(vc, function(x){
    toupper(unlist(strsplit(x, sep)))
  }))
  words <- gsub("[[:punct:]]$", "", words) # get rid of punctuations at end
  words <- gsub("^[[:punct:]]", "", words) # get rid of punctuations at beginning
  
  df <- data.frame(table(words))
  
  # ignore certain words (supports regex pattern!!)
  ignore <- unlist(strsplit(input$n_ins_Wc_ignore, "\\s+"))
  # ignore <- c("GO", "KEGG", "of", "and", "pathway")
  # df <- df[df$words %in% ignore ==F,]
  pattern <- paste(ignore, collapse = "|")
  df <- df[-grep(pattern, df$words, ignore.case=TRUE),]
  
  df
})

n_ins_wc_plt <- reactive({
  df <- n_ins_wc_df()
  req(max(df$Freq)>1) # blocks further processing if no repeated words are found
  
  
  # only draw top x words
  df <- df[order(df$Freq, decreasing = TRUE), ]
  if (nrow(df)>150){
    df <- df[1:150, ] #or set 150 to whatever
  }
  
  # print(head(df))
  wordcloud(df$words, df$Freq, 
            min.freq = 1, max.words=200, scale=c(3,.5),
            random.order=FALSE, 
            rot.per=0.2, # freq of vertical words
            colors=brewer.pal(8, "Dark2"))
})

output$n_ins_wc <- renderPlot({
  req(length(n_ins_fgl())>0)
  
  n_ins_wc_plt()
})

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
            add_help("ins_help", style="margin-left: 5px;"))
          ),
          bsTooltip("ins_help", 
                    "TRUE: fulfills the filters and contained in the gene list;<br>FALSE: NOT contained in the gene list and NOT fulfilling the filters.", 
                    placement = "top"),
          br(),br(),
          uiOutput("ui_intersections"),
          
          HTML(paste0(
            "Active filters:",
            add_help("active_filters_help", style="margin-left: 5px;"))
          ),
          bsTooltip("active_filters_help", 
                    "Filtering conditions for this intersection are displayed below", 
                    placement = "top"),
          uiOutput("intersection_summary")
        ),
    ),
    
    
    
    dataTableOutput("n_ins_tbl"),
    
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          radioGroupButtons(
            inputId = "n_ins_view",
            label = "Choose view:",
            choices = c("Full", "Minimized", "T/F Matrix"),
            selected= "Full", direction="vertical"
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE
        )
        
    ),
    bsTooltip("n_wc_dropdown", "Text enrichment for selected terms", placement = "top"),
    div(style = "position: absolute; left: 4em; bottom: 1em", id="n2_4b",
        dropdown(inputId="n_wc_dropdown",
                 column(8,
                        column(2, textInput("n_ins_wc_sep", "Separator:", value="_")),
                        column(10, textInput("n_ins_Wc_ignore", "Ignore strings: (separated by spaces)", value="and or of GO KEGG WP")),
                        
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
    
    div(style = "position: absolute; left: 7em; bottom: 1em",
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

output$n_ui_intersect <- renderUI({
  # req(rv$n_ui_showpanel == "Intersection")
  req(is.null(rv$df_n)==F)
  
  
  div(
    
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
})

#----------------- venn --------------------
ins_venn_panel <- reactive({
  box(title = span( icon("chart-area"), "Venn Diagram"), status = "primary", solidHeader = F, width=6,
      
      uiOutput("n_venn_ui"),
      
      div(style = "position: absolute; left: 1em; bottom: 1em", 
          dropdown(
            checkboxGroupInput(
              inputId = "n_venn_label",
              label= "Show in label:",
              choices = c("Counts"="counts", "Percent"="percent"),
              selected="counts",
              inline=T, width="250px"
            ),
            radioGroupButtons(
              inputId = "n_venn_type",
              label = "Venn type:",
              choices = c("Basic","Area-proportional"),
              selected= "Basic", direction="horizontal"
            )
            ,
            size = "xs",
            icon = icon("gear", class = "opt"),
            up = TRUE
          )
      ),
      div(style = "position: absolute; left: 4em; bottom: 1em", 
          dropdown(
            downloadButton("n_npvenn_dl", "Download basic"),
            downloadButton("n_venn_dl", "Download area-proportional"),
            
            size = "xs",
            icon = icon("download", class = "opt"),
            up = TRUE, width=300
          )
          
      )
      
      
  )
})

#----------------- upset --------------------
ins_upset_panel <- reactive({
  
  box(
    title = span( icon("chart-area"), "UpSet Plot"), status = "primary", solidHeader = F, width=6,
    
    plotOutput("df_n_upset", width = "100%"),
    
    div(style = "position: absolute; left: 1em; bottom: 1em",
        dropdown(
          selectInput(
            inputId = "n_upset_sortby",
            label = "Order by:",
            choices = c("Frequency"="freq", "Degree"="degree"),
            selected = "freq"),
          materialSwitch(
            inputId = "n_upset_showempty", label = "Show empty intersections?", status="primary",
            value = FALSE
          ),
          
          size = "xs",
          icon = icon("gear", class = "opt"),
          up = TRUE, width=300
        )
        
    ),
    div(style = "position: absolute; left: 4em; bottom: 1em",
        dropdown(
          downloadButton("n_upset_dl", "Download plot"),
          
          size = "xs",
          icon = icon("download", class = "opt"),
          up = TRUE, width=300
        )
        
    )
    
  )
})

output$n_venn_placeholder <- renderUI({
  box(
    title = span( icon("chart-area"), "Venn Diagram"), status = "warning", solidHeader = F, width=6,
    paste0("Venn diagram is only available for 5 or less datasets. You have selected ", length(rv$nx_i)," datasets.")
  )
})
output$n_upset_placeholder <- renderUI({
  box(
    title = span( icon("chart-area"), "UpSet Plot"), status = "warning", solidHeader = F, width=6,
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
      fluidRow(
        div(style="height: 3.5em;",
            column(6,
                   HTML("<span style='font-size: 160%;margin-left: 0.5em;'>Intersection analysis</span>"),
                   
            ),
            column(6,align= "right",
                   div(id="ins_filters_here"),
                   
            )
        )
        
      ),
      fluidRow(
        div(
          column(
            width = 12,
            div(id="n0_4", style="height:400px",
                uiOutput("n_ui_intersect"),
            )
            
          )
        )
      ),
      
      # fluidRow(
      #   column(12,
      #          uiOutput("ins_table_panel")
      #          )
      # )
      # ,
      fluidRow(
        column(12,
               box(
                 width = 12, status = "primary",solidHeader = F,
                 title = span(icon("table"),"Selected Intersection"),
                 div(id= "ins_pg_bottom"),
               )
               
        )
      )
    )
  }
})