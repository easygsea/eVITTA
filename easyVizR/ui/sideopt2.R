# ---------------- only shows when tab2 is selected

sideopt2 <- conditionalPanel(
  condition = "input.tabs == 'tab2'",
  fluidRow(
    column(12,
           div(id="x0_1", uiOutput("select_df"))
           ,
           div(id="x0_2", 
               
               radioButtons(
                 inputId = "mode",
                 label = "Subset data:",
                 choices = c("All genes", "List of genes"))
               
           ),
           conditionalPanel(
             condition = "input.mode == 'List of genes'",
             textAreaInput("genelist_p1", "Input gene list (separated by new line):", "")
           ),
           div(id="x0_3",
               
               conditionalPanel(
                 condition = "input.mode == 'All genes'",
                 actionButton("submit_x", "Visualize!")
               ),
               conditionalPanel(
                 condition = "input.mode == 'List of genes'",
                 actionButton("submit_genelist", "Visualize!")
               )
               
           )
           
    )
    
  ),    
)