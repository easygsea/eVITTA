# ================================================= #
#                     Info box                      ####
# ================================================= #


# ------------- enhanced page progress infobox ------------------
# call in renderUI on the server side. you need to wrap this in a box
# 
# id: id of the list display (<ul>)
# prompt: shown in bold 
# msg: a vector of strings
# conditions: a vector of conditionals (T = condition fulfilled). should be SAME length as msg. 
# button renders only when all conditions are fulfilled.
# bttn_id: id of the button (use in observeEvent to jump to next tab)
# bttn_text: display text of the button
#
# example: 
# progress_box(id="infobox_1", prompt="To-dos:",
# msg=c("1. Search a valid GSE number", "2. Select a platform", "3. Read the study information", "4. This is a sample message", "This is another sample message"), 
# condition=c(!is.null(rv$gse_all), !is.null(rv$plat_id), !is.null(rv$plat_id), !is.null(rv$plat_id),!is.null(rv$plat_id)),
# bttn_id="next_p1", bttn_text="Continue to next panel"
# )

progress_box <- function(id, prompt, msg, condition, bttn_id, bttn_text="Continue to next panel"){
  
  icon <- vector(mode="list", length=length(msg))
  for(i in 1:length(msg)){
    # get icon
    if (condition[[i]]==T){
      icon[[i]] <- "<i class='fa fa-check' style='color:green;'></i>"
      msg[[i]] <- paste0("<span style='color:gray;'><strike>",msg[[i]],"</strike></span>")
    } else {
      icon[[i]] <- "<i class='fa fa-check' style='color:white;'></i>"
    }
  }
  display <- paste0("<li>", msg, icon, "</li>")
  display <- paste0(display, collapse="")
  if (all(condition)){
    bttn <- actionBttn(bttn_id, bttn_text, icon=icon("angle-double-right"), style="simple", color="primary", size="xs")
  } else {bttn <- ""}
  div(
    tags$head(tags$style(
      HTML(paste0("
        #",id," {margin-top:5px; margin-bottom:5px;padding-inline-start: 15px;display:inline;}
        ul#",id," li {display:inline; margin-right:30px;}
        ul#",id," i {margin-left:5px}"))
    )),
    HTML(paste0("<strong>",prompt,"</strong><ul id='",id,"'>", display,"
                  </ul>")),
    bttn
  )
}


# ------- Function to draw an info box to guide the user along the pipeline ---------
# You can pass html string into msg, e.g. : guide_box("<strong>This is a bold message</strong>")
# default color is blue
# default width is 12 (maximum), must be is an integer value
# To make it appear on condition, call it in a conditional renderUI({})
# Then, observeEvent to next tab:
# observeEvent(input$guide1,{updateTabItems(session, "menu1", "tab3")})

guide_box <- function(id,msg, color="warning", size="sm"){
  actionBttn(
    id,
    HTML(msg),
    icon=icon("angle-double-right"),
    style = "simple", color=color, size = size
    , block = T
  )
}



# ================================================= #
#           Stat display replacement                ####
# ================================================= #
# note on "Stat": 
# internally, all data is saved and processed with the Stat column
# however, we do not display "Stat" to the user, because it's confusing
# instead, there are 2 alternatives:
# 1) dynamic: let user tell us what the "Stat" variable is, e.g. logFC/ ES, then we render the corresponding one to them
# 2) static: we decide an arbitrary string to display instead of Stat (i.e. "Value")
# to use the 1st solution, enable the first 2 functions
#       stat_replace1() and stat_replace2()
# to use the 2nd solution, enable the alternative ver of those 2 functions written below
# note: for the "Stat" instances before user specifies it, we will apply a cosmetic change regardless of solution 1 or 2 is used.
#       see the function "stat_replace()"
# to change the default string substitute for "Stat", change this:
stat_replace_string <- "Value"      # <<<-----


############## DYNAMIC STAT

# function 1: replacing the "Stat" substring dynamically
#----------------------------------------------------
# input a string vector to be replaced.
# example: stat_replace1("|Stat| >=:", "dataset1")
# this will replace the Stat in the string with the corresponding rv$tt value for dataset1.
# example output: "|logFC| >=:"
# if there are multiple names with different rv$tt values, will return sth like "|logFC/ES| >=:"

stat_replace1 <- function(vec, selected_x, mode="one", ll=rv$ll, tt=rv$tt){
  # require ll and tt to exist and be of sufficient length
  if (is.null(ll)==F & length(ll)>0 & is.null(tt)==F & length(tt)>0){
    # turn names into rv$ll indices
    selected_i <- unlist(lapply(selected_x, function(x){
      match(x, ll)
    }))
    # print(selected_i)
    replace_strings <- unlist(tt[selected_i]) # get replace strings corresponding to selected indices
    
    if (mode =="one"){ # replace all instances of stat by one string
      replace_strings <- names(table(replace_strings)) # take unique and order by frequency
      stat_replacement <- paste(replace_strings, collapse="/") # render as single string
      out <- gsub("Stat", stat_replacement, vec)
    } else if (mode=="each"){ # replace each instance by differnet strings
      # print(vec)
      # print(replace_strings)
      out <- unlist(lapply(seq_along(vec), function(i){ gsub("Stat", replace_strings[[i]], vec[[i]]) }))
    }
    return(out)
  } else {
    return(vec)
  }
}



# function 2: replace the "Stat" colnames in a df dynamically
#------------------------------------------------------
# simply input the colnames. elements should be in the format: "Stat_dataset1"
# it will match the name behind the underscore to rv$ll and replace the stat substring with corresponding rv$tt value
# example: stat_replace2(colnames(df))

stat_replace2 <- function(colnames, ll=rv$ll, tt=rv$tt){
  # require ll and tt to exist and be of sufficient length
  if (is.null(ll)==F & length(ll)>0 & is.null(tt)==F & length(tt)>0){
    unlist(lapply(colnames, function(x){ # replace the Stat substring based on the attached dataset name
      if (grepl("^Stat_", x)==T){ # if colname contains stat, change it
        name <- gsub("Stat_","",x)
        matched_i <- match(name, ll)
        stat_replacement <- tt[[matched_i]]
        gsub("^Stat", stat_replacement, x)
      } else { # if no, leave it be
        x
      }
    }))
  } else {
    return(colnames)
  }
}

############## STATIC STAT

# or just replace with fixed string 
#------------------------------------------------------

# for single string or vector (comment this out if using dynamic solution)
stat_replace1 <- function(vec, selected_x, mode="one"){
  gsub("Stat", stat_replace_string, vec)
}

# for table colnames (comment this out if using dynamic solution)
stat_replace2 <- function(colnames){
    unlist(lapply(colnames, function(x){ # replace the Stat substring based on the attached dataset name
      if (grepl("^Stat_", x)==T){ # if colname contains stat, change it
        gsub("^Stat", stat_replace_string, x)
      } else { # if no, leave it be
        x
      }
    }))
}


# a general string replacement function  for static instances (static)
stat_replace <- function(vec){
  gsub("Stat", stat_replace_string, vec)
}


# ================================================= #
#                  UI manipulation                  ####
# ================================================= #


# move ui to a certain place
# -----------------------------------
# provide the uiOutput id, id of a selector in desired place, and the relative position

move_ui <- function(uiOutput_id, to_where, relative_pos){
  # remove ui
  removeUI(
    selector = paste0("#", uiOutput_id)
  )
  # print("removed ui")
  
  # insert ui in new location
  insertUI(
    selector = paste0("#", to_where),
    where = relative_pos,
    ui = uiOutput(uiOutput_id)
  )
  # print("inserted ui")
}


# ================================================= #
#                  Filter presets                   ####
# ================================================= #


# initialize filter presets
# ------------------------------------------
sig_icon <- "star-of-life"
both_icon <- "sort"
up_icon <- "arrow-up"
down_icon <- "arrow-down"
default_icon <- "circle-notch"
no_icon <- "times"

sig_txt_color <- "#c463dc"
sig_bg_color <- "#efe7ff"
both_txt_color <- "#217120"
both_bg_color <- "#d6fcd0"
up_txt_color <- "#dd4b39"
up_bg_color <- "#fd8"
down_txt_color <- "#1976d2"
down_bg_color <- "#b7e2ff"
no_txt_color <- "#f4f4f4"
no_bg_color <- "#444"
filter_presets <- list(
  "p significant" = c("psig", 0.05, NA, NA, NA, 
                      "<b>p</b> <= 0.05", 
                      sig_icon, sig_txt_color, sig_bg_color),
  "q significant" = c("qsig", NA, 0.05, NA, NA, 
                      "<b>FDR</b> <= 0.05", 
                      sig_icon, sig_txt_color, sig_bg_color),
  "Changed 0.5x" = c("c0_5", NA, NA, 0.5, "All", 
                     stat_replace("<b>|Stat|</b> >= 0.5 <br><b>Direction</b>: All"), 
                     both_icon, both_txt_color, both_bg_color),
  "Changed 1x" = c("c1", NA, NA, 1, "All", 
                   stat_replace("<b>|Stat|</b> >= 1 <br><b>Direction</b>: All"),
                   both_icon, both_txt_color, both_bg_color),
  "Changed 1.5x" = c("c1_5", NA, NA, 1.5, "All", 
                     stat_replace("<b>|Stat|</b> >= 1.5 <br><b>Direction</b>: All"),
                     both_icon, both_txt_color, both_bg_color),
  "Up 0.5x" = c("up0_5", NA, NA, 0.5, "Positive", 
                stat_replace("<b>|Stat|</b> >= 0.5 <br><b>Direction</b>: +"),
                up_icon, up_txt_color, up_bg_color),
  "Up 1x" = c("up1", NA, NA, 1, "Positive", 
              stat_replace("<b>|Stat|</b> >= 1 <br><b>Direction</b>: +"),
              up_icon, up_txt_color, up_bg_color),
  "Up 1.5x" = c("up1_5", NA, NA, 1.5, "Positive", 
                stat_replace("<b>|Stat|</b> >= 1.5 <br><b>Direction</b>: +"),
                up_icon, up_txt_color, up_bg_color),
  "Down 0.5x" = c("down0_5", NA, NA, 0.5, "Negative", 
                  stat_replace("<b>|Stat|</b> >= 0.5 <br><b>Direction</b>: -"),
                  down_icon, down_txt_color, down_bg_color),
  "Down 1x" = c("down1", NA, NA, 1, "Negative", 
                stat_replace("<b>|Stat|</b> >= 1 <br><b>Direction</b>: -"),
                down_icon, down_txt_color, down_bg_color),
  "Down 1.5x" = c("down1_5", NA, NA, 1.5, "Negative", 
                  stat_replace("<b>|Stat|</b> >= 1.5 <br><b>Direction</b>: -"),
                  down_icon, down_txt_color, down_bg_color),
  "Default" = c("default", 0.05, 1, 0, "All", 
                stat_replace("Default:<br><b>p</b> <= 0.05 <br><b>FDR</b> <= 1<br><b>|Stat|</b> >= 0<br><b>Direction</b>: All"),
                default_icon, no_txt_color, no_bg_color),
  "No filter" = c("nofilter", 1, 1, 0, "All", 
                  stat_replace("Remove all filters:<br><b>p</b> <= 1 <br><b>FDR</b> <= 1<br><b>|Stat|</b> >= 0<br><b>Direction</b>: All"),
                  no_icon, no_txt_color, no_bg_color)
  
)

# apply filter presets to a namespace of filter inputs
#----------------------------------------------------------
# call this in observeEvent to update all the filter inputs under a namespace with values from the presets.
# example: 
# observeEvent(input[[paste0("fpreset_",filter_presets[[1]][[1]])]], {
#   apply_presets_to_filterinputs(1, "f", presets=filter_presets)
# })

apply_presets_to_filterinputs <- function(preset_id, input_ns, presets=filter_presets, input_range=rv$nx_n){
  req(is.null(input_range)==F)
  preset=presets[[preset_id]]
  for (x in 1:length(input_range)){
    if (is.na(preset[[2]])==F){ updateNumericInput(session, inputId=paste0(input_ns,"_p_",x), value=preset[[2]]) }
    if (is.na(preset[[3]])==F){ updateNumericInput(session, inputId=paste0(input_ns,"_q_",x), value=preset[[3]]) }
    if (is.na(preset[[4]])==F){ updateNumericInput(session, inputId=paste0(input_ns,"_Stat_",x), value=preset[[4]]) }
    if (is.na(preset[[5]])==F){ updateRadioGroupButtons(session, inputId=paste0(input_ns,"_sign_",x), selected=preset[[5]]) }
  }
}

# ================================================= #
#                   Filter update                   ####
# ================================================= #
# there are 4 filters: p, q, Stat, sign. first 3 are numericInputs, last is radioGroupButton
# variable name structure is like this:
# nic_p_1, nic_q_1, nic_Stat_1, nic_sign_1...
# Saved filters: rv$nic_...
# filter inputs: input$f_... and input$nic...


# require all filter inputs/ rvs under a namespace
# ------------------------------------------
# can use this inside a observer or render block.
# req call will trigger an error that propagates all the way up to whatever render block or observer is executing.
# example:
# req_filter_ns("f", input) # requires input$f_p_1,...etc
# req_filter_ns("nic", rv) # requires rv$nic_p_1,...etc

req_filter_ns <- function(namespace, var, input_range=rv$nx_n){
  req(is.null(input_range)==F)
  for (i in 1:length(rv$nx_n)){
    req(is.null(var[[paste0(namespace,"_p_",i)]])==F)
    req(is.null(var[[paste0(namespace,"_q_",i)]])==F)
    req(is.null(var[[paste0(namespace,"_Stat_",i)]])==F)
    req(is.null(var[[paste0(namespace,"_sign_",i)]])==F)
  }
}



# update filter inputs using values from given namespace
#----------------------------------------------------------------
# usually rv --> input
# example:
# observeEvent(input$f_reset, {
#   update_filters("f", "nic", rv)
# })
# this basically pulls values in rv$nic_p_1...etc namespace and applies them to the 
# numericInputs and radiogroupbuttons under the input$f_p_1... namespace

update_filters <- function(target_namespace, from_namespace, from_var=rv, input_range=rv$nx_n){
  req(is.null(input_range)==F)
  for (i in 1:length(input_range)){
    updateNumericInput(session, paste0(target_namespace, "_p_",i),
                       value = from_var[[paste0(from_namespace, "_p_",i)]]
    )
    updateNumericInput(session, paste0(target_namespace, "_q_",i),
                       value = from_var[[paste0(from_namespace, "_q_",i)]]
    )
    updateNumericInput(session, paste0(target_namespace, "_Stat_",i),
                       value = from_var[[paste0(from_namespace, "_Stat_",i)]]
    )
    updateRadioGroupButtons(session, paste0(target_namespace, "_sign_",i),
                            selected = from_var[[paste0(from_namespace, "_sign_",i)]]
    )
  }
}

# update rv filters using values from given namespace
# ---------------------------------------------------- 
# usually input --> rv
update_filters_rv <- function(target_namespace, from_namespace, from_var=input, 
                              input_range=rv$nx_n){
  req(is.null(input_range)==F)
  for (i in 1:length(input_range)){
    rv[[paste0(target_namespace, "_p_",i)]] <- from_var[[paste0(from_namespace, "_p_",i)]]
    rv[[paste0(target_namespace, "_q_",i)]] <- from_var[[paste0(from_namespace, "_q_",i)]]
    rv[[paste0(target_namespace, "_Stat_",i)]] <- from_var[[paste0(from_namespace, "_Stat_",i)]]
    rv[[paste0(target_namespace, "_sign_",i)]] <- from_var[[paste0(from_namespace, "_sign_",i)]]
  }
}


# apply css highlights to a namespace of filter inputs based on comparing with another namespace of filter inputs
# ----------------------------------------------
# for instance, to compare input$f_p_1...etc to rv$nic_p_1... etc and apply a highlight,
# we just call this function to get a css object. this must be written into rv:
# rv$f_css_highlights <- observe_filter_highlights("f", input, "nic", rv)
# and then it must be rendered via renderUI/ uiOutput.
# ----------- highlight:
# box-shadow: 0 0 3px red;
# border: 0.1em solid red;
# ----------- normal:
# box-shadow: none;
# border: 1px solid #d2d6de;

observe_filter_highlights <- function(target_namespace, target_var, ref_namespace, ref_var, input_range=rv$nx_n,
                                      # specify css styles
                                      warning_style="{box-shadow: 0 0 3px red;border: 0.1em solid red;}",
                                      normal_textinput_style = "{box-shadow: none;border: 1px solid #d2d6de;}",
                                      normal_radiogroupbuttons_style = "{box-shadow: none;border: #f4f4f4;}"
                                      ){
  highlights <- vector()
  
  for (i in 1:length(input_range)){
    # observe p, q, stat, sign and apply highlights if diff from saved rv value
    if (target_var[[paste0(target_namespace, "_p_",i)]] != ref_var[[paste0(ref_namespace, "_p_",i)]]){
      obs <- paste0("#", paste0(target_namespace, "_p_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0(target_namespace, "_p_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (target_var[[paste0(target_namespace, "_q_",i)]] != ref_var[[paste0(ref_namespace, "_q_",i)]]){
      obs <- paste0("#", paste0(target_namespace, "_q_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0(target_namespace, "_q_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (target_var[[paste0(target_namespace, "_Stat_",i)]] != ref_var[[paste0(ref_namespace, "_Stat_",i)]]){
      obs <- paste0("#", paste0(target_namespace, "_Stat_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0(target_namespace, "_Stat_",i), normal_textinput_style)
      highlights <- c(highlights, obs)
    }
    if (target_var[[paste0(target_namespace, "_sign_",i)]] != ref_var[[paste0(ref_namespace, "_sign_",i)]]){
      obs <- paste0("#", paste0(target_namespace, "_sign_",i), warning_style)
      highlights <- c(highlights, obs)
    } else {
      obs <- paste0("#", paste0(target_namespace, "_sign_",i), normal_radiogroupbuttons_style)
      highlights <- c(highlights, obs)
    }
  }
  # output a css object
  tags$head(tags$style(HTML(
    paste0(highlights, sep=" ")
  )))
}

# ================================================= #
#                   Filter processing               ####
# ================================================= #

# apply a namespace of filters to an input dataframe to get gene lists
#--------------------------------------------------------------------
# example: filter_to_gls("nic", rv, df_n_basic())

filter_to_gls <- function(filter_namespace, filter_var, filtered_df, input_range=rv$nx_n){
  req(is.null(filtered_df)==F)
  req(nrow(filtered_df)>0) # filtered df must not be empty
  req_filter_ns(filter_namespace, filter_var) # check if values exist under filter namespace
  
  df <- filtered_df
  gls <- vector(mode="list") # initialize gls as empty list
  # cutoff according to filters provided for each 
  for (i in 1:length(input_range)){
    n <- input_range[[i]] # name
    ss <- df
    ss <- ss[ss[[paste0("PValue","_", n)]]<=filter_var[[paste0(filter_namespace,"_p_",i)]], ] # filter by p
    ss <- ss[ss[[paste0("FDR","_", n)]]<=filter_var[[paste0(filter_namespace,"_q_",i)]], ] # filter by q
    ss <- ss[abs(ss[[paste0("Stat","_", n)]])>=filter_var[[paste0(filter_namespace,"_Stat_",i)]], ] # filter by stat
    ss <- filter_by_sign(ss, paste0("Stat","_", n), filter_var[[paste0(filter_namespace,"_sign_",i)]], tolerate=T) # filter by stat sign
    
    gl <- as.character(na.omit(ss$Name)) # format into vector genelist
    gls[[n]] <- gl # write into list as named vector
  }
  
  return(gls)
}

# filters a mini table from a gene list
# ------------------------------------
# provide the name of the dataset, gene list, the df to filter
# example: df <- gl_to_table(name = rv$nx_n[[1]], gl = f_temp_gls()[[1]], master_df = rv$df_n, round=3)
# set round to 0 for no rounding

gl_to_table <- function(name, gl, master_df, round=3, keep_stat=F){
  req(is.null(master_df)==F)
  df <- master_df
  show_cols <- c("Name", paste0(c("Stat_", "PValue_", "FDR_"), name))
  df <- df[df$Name %in% gl, show_cols]
  colnames(df) <- c("Name", "Stat", "PValue", "FDR")
  rownames(df) <- NULL
  if (round>0){ 
    df[-1] <- df[-1] %>% mutate_if(is.numeric, ~round(., round)) # round
  }
  # whether stat is kept or changed into a replacement value
  if (keep_stat==F){
    colnames(df) <- stat_replace1(colnames(df), name) # replace stat string
  }
  df
}


# ================================================= #
#        Convert filters to verbal summary          ####
# ================================================= #



# gathers filters about 1 dataset from namespace and summarizes them with a line of HTML text
# -------------------------------------------------
# call this inside HTML()
# name: the name of the dataset
# status: T/F/NA

summarize_filter <- function(filter_namespace, filter_var, name, status, include_name=T, input_range=rv$nx_n){
  req_filter_ns(filter_namespace, filter_var)
  
  i <- match(name, input_range)
  cur_p <- filter_var[[paste0(filter_namespace, "_p_",i)]]
  cur_q <- filter_var[[paste0(filter_namespace, "_q_",i)]]
  cur_Stat <- filter_var[[paste0(filter_namespace, "_Stat_",i)]]
  cur_sign <- filter_var[[paste0(filter_namespace, "_sign_",i)]]
  
  
  req(is.null(status)==F)
  if (is.na(status)==T){ # if NA
    stat_text=""
  } else if (status==F){ # if FALSE
    if (cur_p<1){ # p cutoff is only meaningful if <1
      p_text <- paste0("p > ",cur_p)
    } else {p_text <- NA}
    if (cur_q<1){ # q cutoff is only meaningful if <1
      q_text <- paste0("q > ",cur_q)
    } else {q_text <- NA}
    
    if (cur_sign=="All"){ # if FALSE and ALL
      if (cur_Stat>0){ # |Stat| cutoff is only meaningful if >0
        stat_text <- stat_replace1(paste0("|Stat| < ", cur_Stat), name)
      } else {stat_text <- NA} 
    } else if (cur_sign=="Positive"){ # if FALSE and POS
      stat_text <- stat_replace1(paste0("Stat < ", cur_Stat), name)
    } else if (cur_sign=="Negative") { # if FALSE and NEG
      stat_text <- stat_replace1(paste0("Stat >  ", cur_Stat), name)
    }
  } else if (status==T){ # if TRUE
    if (cur_p<1){
      p_text <- paste0("p <= ",cur_p)
    } else {p_text <- NA}
    if (cur_q<1){
      q_text <- paste0("q <= ",cur_q)
    } else {q_text <- NA}
    
    if (cur_sign=="All"){ # if TRUE and ALL
      if (cur_Stat>0){ # |Stat| cutoff is only meaningful if >0
        stat_text <- stat_replace1(paste0("|Stat| >= ", cur_Stat) , name)
      } else {stat_text <- NA} 
    } else if (cur_sign=="Positive"){ # if TRUE and POS
      stat_text <- stat_replace1(paste0("Stat >= ", cur_Stat), name)
    } else if (cur_sign=="Negative") { # if TRUE and NEG
      stat_text <- stat_replace1(paste0("Stat <=  ", cur_Stat), name)
    }
  } 
  
  if (is.na(status)==F){
    if (status==T){
      cond_string <- paste(
        paste0("<strong>", na.omit(c(p_text, q_text, stat_text)), "</strong>")
        , collapse=" AND ")
      
    } else if (status==F){
      cond_string <- paste(
        paste0("<strong>", na.omit(c(p_text, q_text, stat_text)), "</strong>")
        , collapse=" OR ")
    } 
    
    # assemble into string
    if (include_name==T){
      adddesc <- paste(cond_string, " in ", "<i>",name,"</i>", sep="")
    } else {
      adddesc <- paste(cond_string, sep="")
    }
    
    
  } else {
    adddesc <- NA
  }
  adddesc
}


# ================================================= #
#           More convenience functions              ####
# ================================================= #


# tooltip for radiogroupbutton and radiobuttons
#-----------------------------------------------------
# lets you put separate tooltips on radiogroupbutton and radiobutton choices
# Call this instead of bsTooltip 

radioTooltip <- function(id, choice, title, placement = "bottom", trigger = "hover", options = NULL){
  
  options = shinyBS:::buildTooltipOrPopoverOptionsList(title, placement, trigger, options)
  # options <- c(options, width="100px")
  options = paste0("{'", paste(names(options), options, sep = "': '", collapse = "', '"), "'}")
  bsTag <- shiny::tags$script(shiny::HTML(paste0("
    $(document).ready(function() {
      setTimeout(function() {
        $('input', $('#", id, "')).each(function(){
          if(this.getAttribute('value') == '", choice, "') {
            opts = $.extend(", options, ", {html: true});
            $(this.parentElement).tooltip('destroy');
            $(this.parentElement).tooltip(opts);
          }
        })
      }, 500)
    });
  ")))
  htmltools::attachDependencies(bsTag, shinyBS:::shinyBSDep)
}

# Multiple reqs
#-----------------------------------------------------
# req multiple cols in a df
# example: req_cols(df, c("Name_data1", "Stat_data1", "PValue_data1", "FDR_data1"))
req_cols <- function(df, col_list){
  for (i in col_list){
    req(df[[i]])
  }
}

# req multiple vars to be not NULL
# example: req_vars(c(input$a, input$b, input$c), check_len=T, FUN=length)
# check_len: checks if lengths are >0
req_vars <- function(var_list, check_len=F, FUN=length){
  for (i in var_list){
    req(is.null(var)==F)
    if(check_len==T){
      req(FUN(var)>0)
    }
  }
}

# req a df to exist and contain things
req_df <- function(df){
  req(is.null(df)==F) # exists
  req(nrow(df)>0) # enough rows
  req(ncol(df)>0) # enough cols
}



# update vars from input to rv
#----------------------------------------------------
input2rv <- function(var_list){
  for (var in var_list){
    if(is.null(input[[var]])==F){ 
      rv[[var]] <- input[[var]] 
      }
  }
}



# ================================================= #
#           Intersection extraction                ####
# ================================================= #

# pattern matching of TF vector to a specified pattern (also a TF vector)
# use this for finding genes in an intersection 
#-----------------------------------------------
# input: c(T,F,F), c(T, NA, F)
# output: TRUE
match_skipna <- function(x,pattern, mode="all"){
  match <- na.omit(x==pattern)
  if (mode=="all"){
    return(all(match))
  } else if (mode=="any"){
    return(any(match))
  }
  
}



# extract intersection of several gene lists from a df
#-------------------------------------------------
# out types: "Full"= all the columns; 
# "Minimized"=only Name, Stat, PValue, FDR columns;
# "T/F Matrix"= a true/false matrix only
# include background: whether to include genes not in any gene list

extract_intersection <- function(gls, criteria, df, out_type="Full", include_background=T, partial_match=F){
  req_vars(c(gls, criteria, df))
  req(length(gls)==length(criteria))
  
  if (include_background==T){
    all_genes <- df$Name
  } else if (include_background==F){
    all_genes <- unique(unlist(gls))
  }
  
  # turn gls into list of T/F vectors
  xx <- lapply(seq_along(names(gls)),function(x){
    all_genes %in% gls[[names(gls)[[x]]]]
  })
  names(xx) <- names(gls)
  
  # assemble into a T/F df (the gls matrix)
  glm <- data.frame(xx, row.names = all_genes)
  
  # get subset of genes based on t/f table
  subset <- glm[apply(glm,1,function(x) {
    if (partial_match==F){
      match_skipna(x,criteria, mode="all")
    } else if (partial_match==T){
      match_skipna(x,criteria, mode="any")
    }
    
  }),]
  
  genelist <- rownames(subset) # these are gene list
  
  if (out_type=="Full"){
    out <- df[df$Name %in% genelist,] # extract the rows from full df
  } else if (out_type=="Minimized"){
    xx <- dplyr::select(df, contains(c("Name","Stat", "PValue", "FDR")))
    # print(head(xx))
    out <- xx[xx$Name %in% genelist,]
  } else if (out_type=="T/F Matrix"){
    out <- as.data.frame(subset)
    out <- cbind(rownames(out),out)
    colnames(out)[[1]] <- "Name"
  }
  out
}



# filter df according to a specified dflogic
#-------------------------------------------
# Ins: current intersection
# Both: common intersection in all
# Either: contained in any gene list
# this returns a df that can be used by a plotting function
get_df_by_dflogic <- function(selected, dflogic, gls, user_criteria, starting_df, ref=rv$nx_n){
  # replace all the values in user criteria to get a dummy, all-true criteria
  all_true_criteria <- replace(user_criteria, names(user_criteria), TRUE)
  
  if (dflogic=="Ins"){
    to_plot_df <- extract_intersection(gls = gls, 
                                       criteria = user_criteria, 
                                       df = starting_df, 
                                       out_type = "Full", partial_match=F)
    # print(rv$ins_criteria)
  } else if (dflogic=="Both"){ 
    to_plot_df <- extract_intersection(gls = gls, 
                                       criteria = all_true_criteria, 
                                       df = starting_df, 
                                       out_type = "Full", partial_match=F)
    # print(all_true_criteria)
  } else if (dflogic=="Either"){
    to_plot_df <- extract_intersection(gls = gls, 
                                       criteria = all_true_criteria, 
                                       df = starting_df, 
                                       out_type = "Full", partial_match=T)
    # print(all_true_criteria)
  }
  to_plot_df
}