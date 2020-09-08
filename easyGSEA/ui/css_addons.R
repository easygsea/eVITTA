css_addons <- 
  tags$head(
    tags$style(HTML(paste0(
      
      # fixes large datatables flashing and adds margin on bottom
      #               "#n_ins_tbl{min-height: 480px;margin-bottom: 30px;}
      # #single_tbl{min-height: 480px;margin-bottom: 30px;}
      # #single_gl_tbl{min-height: 480px;margin-bottom: 30px;}"
      
      
      # display help button on right end of row
      "#q1{position: absolute; right: 1em;}"
      
      # dbs conditional panel
      ,"#showdbs_panel{margin-top: 1em;}"
      
      
    )))
  )