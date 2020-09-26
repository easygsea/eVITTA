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




# ================================================= #
#           Stat display replacement                #
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

