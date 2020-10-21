####---------------------- REACTIVE VALUES---------------------------####

rv <- reactiveValues(
                     demo = "", # "yes" for a demo session, NULL for regular runs
                     demo_n = 1, # odd for load, even for unload
                     
                     upload_state = NULL, 
                     # ll=ll, gg=gg, tt=tt,
                     ll=list(), gg=list(), tt=list(),
                     upload_columns=NULL, df=NULL,
                     genelist=NA,
                     FileDF = data.frame(matrix(ncol=5,nrow=0, dimnames=list(NULL, c("name", "size", "type", "datapath", "tidiedName"))))
                     
)