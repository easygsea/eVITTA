#=============================================================# 
######                   DOWNLOAD RESULTS              ########
#=============================================================#
# Overall UI download ----------------
output$ui_downloadlist <- renderUI({
    req(rv$run == "success")
    
    fluidRow(
        column(12,
               tags$hr(style="border-color: #48617b;margin: 8px;"),
               uiOutput("menu_download_table"),
               tags$hr(style="border-color: #48617b;margin: 8px;")
        )
    )

    # box(
    #     width = 12, status = "primary",
    #     uiOutput("menu_download_table")
    # )
})

# UI download box
output$ui_downloadbox <- renderUI({
    box(
        title = span(icon("table"),"Enrichment Results Table"), width = 12, status = "primary",
        
        if(is.null(rv$run) || rv$run != "success"){
            fluidRow(
                box(
                    title = span( icon("exclamation"), "Notification"), status = "warning", width=12,
                    "Results table available upon successful run."
                )
            )
            
        }else{
            fluidRow(
                column(12,
                    wellPanel(
                        style = paste0("background:",bcol1),
                        
                        fluidRow(
                            column(12, align="center",
                                   p(HTML("Download enrichment table and proceed to <a href='http://tau.cmmt.ubc.ca/eVITTA/easyVizR/' target='_blank'><u><b>easyVizR</b></u></a> for multiple comparisons on functional categories")),
                                   
                                   # div(
                                   #     style="display: inline-block;vertical-align:top;",
                                       downloadBttn("gs_tbl_dl",
                                                    label = "Download enrichment table (.csv)"
                                                    , style = rv$dbtn_style, color = "warning"
                                                    , size="md", block = F
                                       )
                                   # ,bsTooltip("gs_tbl_dl",HTML("Download converted DEG table and proceed to <b>easyVizR</b> for multiple comparisons on functional categories")
                                   #            ,placement = "top")
                                   # ),
                                   # div(
                                   #     style="display: inline-block;vertical-align:top;",
                                   #     uiOutput("ui_tl_cut")
                                   # )
                            )
                        )
                        
                        
                        
                    )
                ),
                column(12,
                    dataTableOutput("selected_es_tables"),style="font-size:75%"
                )
            )
        }
        
    )
})

# database selection box
gs_selected <- reactive({
    gs_selected <- input$selected_download_gs
    if (is.null(gs_selected))
        return(NULL)
    else
        return(gs_selected)
})

output$menu_download_table <- renderUI({
    req(rv$run == "success")
    
    if(input$selected_species != "other"){
        dbs = rv$dbs
    }else{
        dbs = rv$gmt_cs
    }
    
    checkboxGroupInput("selected_download_gs", label = div(style="font-weight:400;", "Select to download:"),
                       choices = dbs,
                       selected = dbs)
})

# ----------UI table cut-------------
output$ui_tl_cut <- renderUI({
    dropdownButton(
        sliderTextInput("cutoff_p_tl",
                        label = "P threshold:",
                        choices= cutoff_slider,
                        selected=rv$tl_p, grid=T, force_edges=T
        ),
        sliderTextInput("cutoff_q_tl",
                        label = "P.adj threshold:",
                        choices= cutoff_slider,
                        selected=rv$tl_q, grid=T, force_edges=T
        ),
        radioGroupButtons(
            inputId = "up_or_down_tl",
            label = "Direction of change:",
            choiceNames = c("Up", "Down", "Both"),
            choiceValues = c("up", "down", "both"),
            selected = rv$tl_ES,
            direction = "horizontal"
        ),
        br(),
        bsButton(
            inputId = "confirm_tl",
            label = span(icon("cut"),"Cut table"),
            style = "primary"
        ),
        width = "300px",circle = TRUE, status = "warning",
        size = "xs",
        icon = icon("fas fa-cut"),# class = "opt"),
        up = FALSE,
        tooltip = tooltipOptions(title = "Click to filter table")
    )
})

#-----------observe cut table events---------------
observeEvent(input$confirm_tl,{
    rv$tl_p = input$cutoff_p_tl
    rv$tl_q = input$cutoff_q_tl
    rv$tl_ES = input$up_or_down_tl
    
})

#-----------render enrichment data table---------------

output$selected_es_tables <- DT::renderDataTable({
    req(is.null(gs_selected())==FALSE)
    req(rv$run == "success")
    # df = filter_df()
    df = combine_df()
    df <- df %>%
        mutate_if(is.numeric, function(x) round(x, digits=3))
    df
    # DT::datatable(df,
    #               extensions=c('Scroller'),
    #               options = list(
    #                   scrollY = "155px",
    #                   scroller = TRUE,
    #                   scrollX=TRUE           
    #               ))
    
    }, plugins="ellipsis", options = dt_options()
)

output$gs_tbl_dl <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_",paste(input$selected_download_gs,collapse="-"),".csv")},
    content = function(file) {
        # df <- filter_df()
        df = combine_df()
        fwrite(df, file, sep=",", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=T)
    })


# ------------ UI GMT download --------------

output$ui_gmt_download <- renderUI({
    if(input$selected_species == ""){
        fluidRow(
            box(
                title = span( icon("exclamation"), "Notification"), status = "warning", width=12,
                "Select your species of interest to download."
            )
        )
    }else if(input$selected_species == "other"){
        p("You have uploaded your own GMT libraries.")
    }else{
        species <- input$selected_species
        species_full <- species_translate(species)
        
        # get all GMT file paths
        gmt_paths = unname(unlist(gmt_collections_paths[[species]],recursive = T))
        gmt_paths = gsub(paste0(getwd(),"/www/"),"",gmt_paths)
        gmt_paths_basenames = paste0(gsub(" ","_",species_full),"_",basename(gmt_paths))
        
        a_links = paste0("<a href='",gmt_paths,"' download> <i class='fa fa-download'> </i>",gmt_paths_basenames,"</a><br/>")
        
        div(
            p("All our gene set libraries (.GMT) are available for download for further analysis and for tool development."),
            p("Please acknowledge our work if you use one of our library files."),
            do.call(HTML,as.list(a_links))
        )
    }
})

# --------- navigation button to return to network --------
output$download_b_btn <- renderUI({
    req(rv$run == "success")
    
    div(
        nav_btn_b("download_b")
        ,bsTooltip("download_b",HTML("Return to <b>Enrichment Network</b>"))
    )
})

observeEvent(input$download_b,{
    updateTabItems(session, "tabs", "network")
})
    
