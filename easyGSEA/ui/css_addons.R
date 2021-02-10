css_addons <- 
  tags$head(
    tags$style(HTML(paste0(
      
      # fixes large datatables flashing and adds margin on bottom
      #               "#n_ins_tbl{min-height: 480px;margin-bottom: 30px;}
      # #single_tbl{min-height: 480px;margin-bottom: 30px;}
      # #single_gl_tbl{min-height: 480px;margin-bottom: 30px;}"
      
      
      # display help button
      # "#q1{position: absolute; right: 1em;}" # displays at right side of row
      "#q1{
      # margin-left: 0.3em !important; 
            margin-top:3px;
            width:16px !important; 
            height:16px !important; 
            padding:0 0 0 1px !important; 
            vertical-align:baseline !important; 
            font-size:10px !important;
            border-color:transparent;
            position: absolute; right: 1em;
      }" # displays after text
      
      # dbs conditional panel
      ,"#showdbs_panel{margin-top: 1em;}"
      
      
      # reset button
      ,"#reset {margin-top:-25px;margin-bottom:5px;}"
      
      # numeric identifier
      ,"#num_acc_hp {height:}"
      
      # GMT upload area
      ,"#drop-area {
      background-color:#ffffe6;
      border:1px dashed #48617b;
      border-radius:25px;
      height:90px;
      overflow:auto;
      margin-bottom:10px;
      }"
      
      # GMT upload button
      ,"#gmt_cc {
      color: black;
      background-color:#ffffe6;
      border:1px dashed #48617b;
      border-radius:25px;
      height:90px;
      overflow:auto;
      }"
      
      ,"#gmt_cc:hover {
      background-color: #e6e6ff;
      border:1px solid #e6e6ff;
      box-shadow: 0 0 .3em #e6e6ff;
      }"
      
      # GMT upload progress bar
      ,"#gmt_c_progress {
      }"
      
      # place the help button on top of the dataoutput
      ,"#cluster_df{ position: relative; z-index: 0; }"

      # file upload in GMT area
      ,"#drag_gmt {
      margin-top:-8px;
      }"
    )))
  )