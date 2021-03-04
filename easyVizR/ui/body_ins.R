# -------------- TAB 3 -------------------

body_ins <- tabItem(tabName = "tab_ins",
                # div(id="ins_main_panels_here"),
                 uiOutput("ins_main_panels"),
                 
                 fixedPanel(
                   style = "z-index:9999",
                   uiOutput("i_floating_buttons"),
                   right = 30,
                   bottom = 30
                 )
                 
                 
)


