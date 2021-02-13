# -------------- TAB 3 -------------------

body3 <- tabItem(tabName = "tab3",
                 
                 fluidRow(
                   div(
                     id = "n_main_panel",
                     column(
                       width = 12,
                       div(id="n0_4", style="height:400px",
                           # uiOutput("n_header"),
                           uiOutput("n_panels")
                       )
                       
                     )
                   )
                 )
                 ,
                 fixedPanel(
                   uiOutput("n_floating_buttons"),
                   right = 30,
                   bottom = 30
                 )
                 
                 
)


