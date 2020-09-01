# ---------------- only shows when tab3 is selected

sideopt3 <- conditionalPanel(
  condition = "input.tabs == 'tab3'",
  fluidRow(
    column(12,
           div(id="n0_1", uiOutput("select_df_p2")),
           div(id="n0_2", style="height:60px",
               uiOutput("n_shared")
           ),
           
           div(id="n0_3",
               actionButton("n_use_data", "Visualize!")
           )
           
    )
  ),
  
)