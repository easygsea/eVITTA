####---------------------- Events ---------------------------####

# update variables
observe({
  if(is.null(input$df_x_p)==F) {rv$df_x_p <- input$df_x_p}
  if(is.null(input$df_x_q)==F) {rv$df_x_q <- input$df_x_q}
  if(is.null(input$df_x_Stat)==F) {rv$df_x_Stat <- input$df_x_Stat}
  
  if(is.null(input$fs_volcano_p)==F) {rv$fs_volcano_p <- input$fs_volcano_p}
  if(is.null(input$fs_volcano_Stat)==F) {rv$fs_volcano_Stat <- input$fs_volcano_Stat}
  
  if(is.null(input$df_gl_p)==F) {rv$df_gl_p <- input$df_gl_p}
  if(is.null(input$df_gl_q)==F) {rv$df_gl_q <- input$df_gl_q}
  if(is.null(input$df_gl_Stat)==F) {rv$df_gl_Stat <- input$df_gl_Stat}
  
  if(is.null(input$gl_volcano_p)==F) {rv$gl_volcano_p <- input$gl_volcano_p}
  if(is.null(input$gl_volcano_Stat)==F) {rv$gl_volcano_Stat <- input$gl_volcano_Stat}
  
  if(is.null(input$p1_bar_data)==F) {rv$p1_bar_data <- input$p1_bar_data}
  if(is.null(input$p1_bar_sig)==F) {rv$p1_bar_sig <- input$p1_bar_sig}
  
})

# update current view
current<- reactive({
  if (is.null(input$show_df)){return(1)}
  else {return({match(input$show_df,rv$ll)})}
})




# submit full data
observeEvent(input$submit_x,{
  #print("submit full data: mode")
  
  rv$mode <- isolate(input$mode)
  withProgress(message = 'Updating data...', value = 0, {
    incProgress(0.2)
    
    df <- isolate(rv$gg[[current()]])
    # set data type
    df$Name <- as.character(df$Name) # convert name column to character
    df[-1] <- sapply(df[-1],as.numeric) # convert all but name to numeric
    
    
    
    incProgress(0.2)
    
    # initialize graphing params
    rv$show_df <- isolate(input$show_df)
    rv$x_i <- isolate(current())
    
    rv$df_x_p <- 0.05
    rv$df_x_q <- 1
    rv$df_x_Stat <- 0
    
    rv$fs_volcano_p <-0.05
    rv$fs_volcano_Stat <-0
    rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
    rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
    incProgress(0.2)
    
    rv$df <- df
    incProgress(0.2)
  })
})

# submit gene list
observeEvent(input$submit_genelist,{
  #print("submit genelist: mode")
  
  rv$mode <- isolate(input$mode)
  rv$genelist <- isolate(as.list(strsplit(input$genelist_p1, '\\n+'))) 
  withProgress(message = 'Updating data...', value = 0, {
    incProgress(0.2)
    
    df <- isolate(rv$gg[[current()]])
    incProgress(0.2)
    
    # initialize graphing params
    rv$show_df <- isolate(input$show_df)
    rv$x_i <- isolate(current())
    
    rv$df_gl_p <- 1
    rv$df_gl_q <- 1
    rv$df_gl_Stat <- 0
    
    rv$gl_volcano_p <-0.05
    rv$gl_volcano_Stat <-0
    rv$volcano_xmax <- max(abs(df$Stat), na.rm = TRUE)+0.5 # find max of Stat
    rv$volcano_ymax <- max(-log10(df$PValue), na.rm = TRUE)+0.5 # find max of -log10p
    
    # bar params
    rv$gl_cols <- isolate(colnames(rv$gg[[current()]])[-1])
    rv$p1_bar_data <- rv$gl_cols[[1]]
    rv$p1_bar_sig <- "PValue"
    
    incProgress(0.2)
    
    # trim by gene list
    #print(rv$genelist[[1]])
    df <- df[df$Name %in% rv$genelist[[1]],]
    df <- df[order(match(df$Name, rv$genelist[[1]])), ]
    incProgress(0.2)
    #print(head(df))
    rv$df <- df
    
  })
})