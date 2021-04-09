
options(shiny.maxRequestSize=100*1024^2) # sets max upload size to 100 mb

# Define server logic required to draw a histogram
shinyServer(function(input, output, session) {
    # initialize reactive values
    source("server/rv.R", local = TRUE)
    
    # RAM usage check on initialization
    source("server/server-ramCheck.R", local = TRUE)
    
    # codes to initiate a demo run. Open the demo.R file, uncomment all rows of scripts to enter the demo mode
    source("server/demo.R", local = TRUE)
    
    # Intro Tour
    source("server/help.R", local = TRUE)

    waiter_hide() # will hide *on_load waiter

    # toggle button for a demo run
    output$btn_demo <- renderUI({
        req(is.null(rv$demo_yes))
        btn_demo("ee")
    })
    
    observeEvent(input$ee,{
        btn_demo_e()
    })

    ####---------------------- HEADER DROPDOWN: SAMPLES SELECTED  ---------------------------####
    # this is the top right notification button on header
    # that shows your currently selected samples
    # (SHOULD STAY IN SERVER.R)
    
    
    dropdown_report <- reactive({
        # req(is.null(rv$gse_all)==F)
        # req(is.null(rv$plat_id)==F)
        # req(is.null(rv$samples)==F)
        # req(is.null(rv$pdata)==F)
        
        if (is.null(rv$gse_all)==F & is.null(rv$plat_id)==F & is.null(rv$samples)==F & is.null(rv$pdata)==F){
            to_show <- translate_sample_names(rv$samples,  # translating from
                                              rv$pdata[,c("title", "geo_accession")],  # translation df
                                              "title") # translating to
            
            if (length(to_show)>30){
                to_show_txt <- paste0(paste(to_show[1:30], collapse= ", "), "<br><i>... and ", length(to_show)-30 ," more</i>")
            } else {
                to_show_txt <- paste(to_show, collapse= ", ")
            }
            
            
            customSentence <- function(numItems, type) {
                shiny::HTML(
                    paste0("<strong>Samples selected (", length(to_show),"): </strong><br>",
                           to_show_txt)
                )
            }
        } else {
            customSentence <- function(numItems, type) {
                shiny::HTML(
                    paste0("<strong>No samples selected.</strong>")
                )
            }
        }
        
    })
    
    # actually render the dropdownMenu
    output$dropdown_menu <- renderMenu({
        
        dropdownMenuCustom(type = "tasks",
                           customSentence = dropdown_report()
        )
    })

    ####---------------------- 1. LOAD FROM GEO  ---------------------------####
    
    
    source("server/1.1_load_geo.R", local = TRUE)
    
    # rv$geo_accession
    # rv$gse_all
    # rv$platforms
    # rv$plat_id
    # study_type() $type $channel_count
    # rv$all_samples # all samples
    # initializes rv$samples # filtered samples
    # rv$pdata = pData(phenoData(gse()))
    # initializes rv$fddf # filtered design table
    # initializes rv$dmdf # data matrix table
    
    source("server/1.2_show_metadata.R", local = TRUE)
    
    # gse() 
    # gse_meta()
    # gse_meta_df()
    # gse_samples()
    # gsm_meta()
    # gsm_meta_df()
    # all_fields() 
    
    
    ####---------------------- 2. DESIGN SUMMARY  ---------------------------####
    
    
    source("server/2.1_design_summary.R", local = TRUE)
    
    # design_df()
    # var_summary()
    
    
    source("server/2.2_filter_design.R", local = TRUE)
    
    # filtered_design_df()
    # updates rv$samples
    # updates rv$fddf

    
    ####---------------------- 3. DATA MATRIX  ---------------------------####
    
    
    source("server/3_data_matrix.R", local = TRUE)
    
    # rv$sup_source
    # filtered_data_df()
    # updates rv$dmdf
    

    #This part accompanies refresh button in modal dialog
    
    ####---------------------- 4.1. SELECT COMPARISON  ---------------------------####
    
    source("server/4.1_select_comparison.R", local = TRUE)
    
    # input$sp_select_levels
    # input$sp_select_var
    # input$sp_batch_col
    
    
    ####---------------------- 4.2. CONFIRM DATA MATRIX  ---------------------------####
    
    source("server/4.2_confirm_matrix.R", local = TRUE)
    
    # rv$matrix_ready
    

    ####---------------------- 4.3. RUN DEG ANALYSIS  ---------------------------####
    
    source("server/4.3_run_deg.R", local = TRUE)
    

    
    ####---------------------- 5. VISUALIZE DEG RESULTS  ---------------------------####
    
    
    source("server/5_vis.R", local = TRUE)
    

    
    ####-------------------00: FUNCTIONS: plots && variables-----------------####
    
    
    source("server/functions.R", local = TRUE)
    source("server/server-variables.R", local = TRUE)
    
    
    ###### ---------------- NOTES ---------------- ######
    
    # Conditions for running limma: (button only shows when all fulfilled)
    # - 2 or more levels selected for comparison
    # - count matrix is uploaded
    # - columns corresponding to the filtered samples are present in the count matrix, AND do not contain *any* NA values
    # - the selected variable is not the same as selected batch effect column
    # 
    # 
    # The following reactives/ variables are provided.
    # - full count matrix: rv$dmdf
    # - filtered count matrix: filtered_data_df()
    # - full sample list: rv$all_samples
    # - filtered sample list: rv$samples
    # - full design matrix: design_df()
    # - filtered design matrix: rv$fddf
    # - full design matrix: rv$fddf_o
    # - data matrix sample names(first row of the df): rv$dmdf_samples
    # - design matrix sample names(first column of the df): rv$fddf_samples
    # 
    # params:
    #     - selected variable: input$sp_select_var
    # - selected 2 levels: input$sp_select_levels
    # - batch effect col: input$sp_batch_col
    # - raw or normalized counts: input$data_type
    # 
    # Metadata:
    #     - full phenoData: rv$pdata
    # - GSE metadata as list: gse_meta()
    # - GSE metadata as df: gse_meta_df()
    # - GSM metadata as list: gsm_meta() 
    # (this only contains fields shared among all samples. e.g. organism. To access other fields that differ between samples, do subsetting on rv$pdata directly.) 
    # - GSM metadata as df: gsm_meta_df()
    # - a master df for gsm and gse metadata: all_fields() 
    # 
    # Others:
    #     - gse object for current platform: gse()
    # - summary of variables and levels: var_summary()
    # (is a named list of named vectors in form of $var level=freq)
    
    
    ####---------------------- DEBUG 1 ---------------------------####
    
    output$debug0 <- renderPrint({
        paste("rv$platforms = ", rv$platforms, ", "
              ,"rv$plat_id = ", rv$plat_id, ", "
        )
    })
    
})
