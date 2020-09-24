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


# initialize filter presets
# ------------------------------------------
filter_presets <- list(
  "p significant" = c("psig", 0.05, NA, NA, NA),
  "q significant" = c("qsig", NA, 0.05, NA, NA),
  "Changed 0.5x" = c("c0.5", NA, NA, 0.5, "All"),
  "Changed 1x" = c("c1", NA, NA, 1, "All"),
  "Changed 1.5x" = c("c1.5", NA, NA, 1.5, "All"),
  "Up 0.5x" = c("up0.5", NA, NA, 0.5, "Positive"),
  "Up 1x" = c("up1", NA, NA, 1, "Positive"),
  "Up 1.5x" = c("up1.5", NA, NA, 1.5, "Positive"),
  "Down 0.5x" = c("down0.5", NA, NA, 0.5, "Negative"),
  "Down 1x" = c("down1", NA, NA, 1, "Negative"),
  "Down 1.5x" = c("down1.5", NA, NA, 1.5, "Negative"),
  "No filter" = c("nofilter", 1, 1, 0, "All")
)



# ================================================= #
#           Stat display replacement                #
# ================================================= #

# function 1: replacing the "Stat" substring
#----------------------------------------------------
# input a string vector to be replaced.
# example: stat_replace1("|Stat| >=:", "dataset1")
# this will replace the Stat in the string with the corresponding rv$tt value for dataset1.
# example output: "|logFC| >=:"
# if there are multiple names with different rv$tt values, will return sth like "|logFC/ES| >=:"

stat_replace1 <- function(vec, selected_x, ll=rv$ll, tt=rv$tt){
  # require ll and tt to exist and be of sufficient length
  if (is.null(ll)==F & length(ll)>0 & is.null(tt)==F & length(tt)>0){
    # turn names into rv$ll indices
    selected_i <- unlist(lapply(selected_x, function(x){
      match(x, ll)
    }))
    # print(selected_i)
    replace_strings <- unlist(tt[selected_i]) # get replace strings corresponding to selected indices
    # print(replace_strings)
    replace_strings <- names(table(replace_strings)) # take unique and order by frequency
    # print(replace_strings)
    stat_replacement <- paste(replace_strings, collapse="/") # render as single string
    out <- gsub("Stat", stat_replacement, vec)
    return(out)
  } else {
    return(vec)
  }
}

# function 2: replace the "Stat" colnames in a df
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
