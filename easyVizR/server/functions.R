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

