####---------------------- REACTIVE VALUES---------------------------####

rv <- reactiveValues(upload_state = NULL, 
                     # ll=ll, gg=gg, tt=tt,
                     ll=list(), gg=list(), tt=list(),
                     upload_columns=NULL, df=NULL,
                     genelist=NA
)