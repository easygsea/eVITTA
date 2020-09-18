# -------------- TAB 3 -------------------

body_ins <- tabItem(tabName = "tab_ins",
                 uiOutput("ins_main_panels"),
                 
                 fixedPanel(
                   uiOutput("i_floating_buttons"),
                   right = 30,
                   bottom = 30
                 )
                 
                 
)


