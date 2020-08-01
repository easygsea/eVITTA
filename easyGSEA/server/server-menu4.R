#=============================================================# 
######                   DOWNLOAD RESULTS              ########
#=============================================================#
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
    checkboxGroupInput("selected_download_gs", label = tags$b("Select to download:"),
                       choices = rv$dbs,
                       selected = rv$dbs)
})

# ----------UI table cut-------------
output$ui_tl_cut <- renderUI({
    dropdown(
        sliderTextInput("cutoff_p_tl",
                        label = "P threshold:",
                        choices= c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1),
                        selected=rv$tl_p, grid=T, force_edges=T
        ),
        sliderTextInput("cutoff_q_tl",
                        label = "P.adj threshold:",
                        choices= c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.1,0.25,0.3,0.5,1),
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
            label = "Cut table",
            style = "success"
        ),
        size = "l",
        icon = icon("fas fa-cut"),# class = "opt"),
        up = FALSE
    )
})

#-----------observe cut table events---------------
observeEvent(input$confirm_tl,{
    rv$tl_p = input$cutoff_p_tl
    rv$tl_q = input$cutoff_q_tl
    rv$tl_ES = input$up_or_down_tl
    
})

output$selected_es_tables <- DT::renderDataTable({
    req(is.null(gs_selected())==FALSE)
    req(rv$run == "success")
    df = filter_df()
    df <- df %>%
        mutate_if(is.numeric, function(x) round(x, digits=3))
    return(df)
    },options = list(pageLength = 1, scrollX=TRUE)
)

output$gs_tbl_dl <- downloadHandler(
    filename = function() {paste0(rv$rnkll,"_",paste(input$selected_download_gs,collapse="-"),".csv")},
    content = function(file) {
        df <- filter_df()
        fwrite(df, file, sep=",", 
               # sep2=c("", ";", ""), 
               row.names = F, quote=T)
    })

output$ui_gmt_download <- renderUI({
    req(input$selected_species)
    species <- input$selected_species
    species_full <- species_translate(species)
    
    # get all GMT file paths
    gmt_paths = unname(unlist(gmt_collections_paths[[species]],recursive = T))
    gmt_paths = gsub(paste0(getwd(),"/www/"),"",gmt_paths)
    gmt_paths_basenames = paste0(gsub(" ","_",species_full),"_",basename(gmt_paths))
    
    a_links = paste0("<a href='",gmt_paths,"' download> <i class='fa fa-download'> </i>",gmt_paths_basenames,"</a><br/>")
    do.call(HTML,as.list(a_links))
})
    
    
