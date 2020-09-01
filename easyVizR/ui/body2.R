# -------------- TAB 2 -------------------

body2 <- tabItem(tabName = "tab2",
                 
                 
                 fluidRow(
                   div(
                     id = "single_main_panel",
                     column(
                       width = 12,
                       
                       div(id="x0_4",style="height:400px",
                           uiOutput("x_header"),
                           uiOutput("single_panels")
                       )
                       
                     )
                   )
                 )
                 ,
                 fixedPanel(
                   uiOutput("x_floating_buttons"),
                   right = 30,
                   bottom = 30
                 )
                 
)