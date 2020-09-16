css_addons <- 
  tags$head(
    tags$style(HTML(paste0(
      ".box.box-solid.box-primary {
          border: 1px solid #1976d2;
      }",
      
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