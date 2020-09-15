css_addons <- 
  tags$head(
    tags$style(HTML(paste0(
      
      "
      .navbar-custom-menu>.navbar-nav>li>.dropdown-menu {
      width:400px;
      }
      ",
      "#select_plat .tooltip {width: 300px;}
      .tooltip-inner {
    text-align: left;}"
      
      
    )))
  )