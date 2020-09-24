#Added a boolean to check if memory limit message has been shown
#FirstTimeMemLimitMessage <- reactiveVal(TRUE)


#observe({
# Re-execute this reactive expression after 1000 milliseconds
#invalidateLater(1000, session)

mem = mem_used()
#This is the part to decide the threshold, and we can try different values later
#if(mem < 50000 & FirstTimeMemLimitMessage()){
if(mem < 1500000){
  rv$mem_n = 1
  
  showModal(modalDialog(
    title = NULL,
    fluidRow(
      column(12, style="font-size:200%;", align = "center",
             p("eVITTA is experiencing high traffic at the moment.")
             ,br(),p("If you have any other unused eVITTA session(s) running, kindly close the window(s).")
             ,br(),p("Email us at evitta@cmmt.ubc.ca if you continue seeing this message. We appreciate your support.")
             # ,br(),p("Thank you for your support.")
             ,br(),p("Please refresh your page and try again")
             ,HTML("<a href='https://tau.cmmt.ubc.ca/eVITTA/easyGSEA'>Refresh</a>")
      )
    )
    ,size = "l"
    ,easyClose = F
    ,footer = NULL #modalButton("Dismiss")
  ))
  # FirstTimeMemLimitMessage(FALSE)
  
}

observeEvent(rv$mem_n,{
  app = "GSEA"
  # send notification email to evitta@cmmt.ubc.ca
  e_src = sprintf("echo '%s;%s;%s' | mail -s 'eVITTA - out of memory %s' evitta@cmmt.ubc.ca"
                  ,Sys.time(),app,mem,Sys.time()
                  )
  system(e_src)
  
  # record the disconnection and write out to report table
  odir = paste0(getwd(),"/bug_report/")
  ofile = paste0(odir,"out_of_ram_report.csv")
  oline = paste0("\"",Sys.time(),"\"",",\"",app,"\",\"OOR\",\"",mem,"\"") # OOR = Out Of RAM
  write(oline, file=ofile, append = T)
  
  # # simulate closing sessions
  # session$close()
  
})

# # stop app when session ends
# session$onSessionEnded(function(){
#   stopApp()
# })
#})
#End of memory usage part
