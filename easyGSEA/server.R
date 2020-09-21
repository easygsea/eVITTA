# sets max upload size to 500MB, modal appears when it exceeds 50MB,
# see 1.1.server-run.R line 325 for more details
options(shiny.maxRequestSize=500*1024^2) 

server <- function(input, output, session) {
    waiter_hide() # will hide *on_load waiter
    
    #Added a boolean to check if memory limit message has been shown
    FirstTimeMemLimitMessage <- reactiveVal(TRUE)
    
    observe({
        # Re-execute this reactive expression after 1000 milliseconds
        invalidateLater(1000, session)
        #garbage collect and check the memory threshold every 1 second
        # gc()
        mem = mem_used()
        #This is the part to decide the threshold, and we can try different values later
        if(mem < 50000 & FirstTimeMemLimitMessage()){
            showModal(modalDialog(
                title = NULL,
                fluidRow(
                    column(12, style="font-size:150%;", align = "center",
                        p("eVITTA is experiencing high traffic at the moment.")
                        ,br(),p("If you have any other unused eVITTA session(s) running, kindly close the window(s).")
                        ,br(),p("Email us at evitta@cmmt.ubc.ca if you continue seesing this message. We appreciate your support.")
                        ,br(),p("Please refresh your page and try again")
                        ,HTML("<a href='https://tau.cmmt.ubc.ca/eVITTA/easyGSEA'>Refresh</a>")
                        
                    )
                ),
                size = "l",
                easyClose = F,
                footer = NULL
            ))
            FirstTimeMemLimitMessage(FALSE)
            
            # record the disconnection and write out to report table
            odir = paste0(getwd(),"/bug_report/")
            ofile = paste0(odir,"out_of_ram_report.csv")
            oline = paste0("\"",Sys.time(),"\"",",\"OOR\",\"",mem,"\"")
            write(oline, file=ofile, append = T)
            
            # simulate closing sessions
            session$close()
        }
    })
    #End of memory usage part
    
    # addClass(selector = "body", class = "sidebar-collapse")
    runjs("$('#rnkfile').parent().removeClass('btn-default').addClass('btn-danger');")
    
    # js$hidehead('none')
    
    source("server/server-rv.R", local = TRUE)
    source("server/server-functions.R", local = TRUE)
    source("server/1.1.server-run.R", local = TRUE)
    source("server/1.2.server-body1.R", local = TRUE)
    
    source("server/2.server-results.R", local = TRUE)
    # source("server/server-summary.R", local = TRUE)
    source("server/3.server-network.R", local = TRUE)
    source("server/4.server-download.R", local = TRUE)
    
    col_f = c("01_WormCat (Holdorf et al. 2020)","02_Pathway","03_Gene Ontology")
    
    #===================== GMT collections =====================
    # initialize three list vectors
    # 1. a list vector to store paths to database collection (.GMT) files
    gmt_collections_paths <- vector("list")
    # 2. a list vector to store database collection categories and names
    gmt_collections <- vector("list")
    # 3. a list vector to store default selected database collections categories and names
    gmt_collections_selected <- vector("list")
    
    # read in the file which stores GMTs to load
    test = read.csv(paste0(getwd(),"/www/gmts/gmts_list.csv"),header=F,stringsAsFactors = F)
    
    # names of databases
    dbs = strsplit(test$V3,";")
    for(i in seq_along(dbs)){
        # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
        names <- gsub(".gmt$","",dbs[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
        names_abbr = abbreviate_string(names); coll = names_abbr
        names(coll) = names
        
        # store databases names into the list vector that stores collections
        gmt_collections = c(gmt_collections, list(coll))
        
        # paths to GMT files
        paths = paste0(getwd(),"/www/gmts/",test$V1[[i]],"/",test$V2[[i]],"/",dbs[[i]])
        names(paths) = names_abbr
        
        gmt_collections_paths = c(gmt_collections_paths, list(paths))
        
        
    }
    
    # name collections by species names
    # gmt_collections = Map(setNames, as.list(gmt_collections), names)
    names(gmt_collections) = test$V2
    gmt_collections = split(gmt_collections,test$V1)
    
    # name collection (.GMT) paths by species names
    # gmt_collections_paths = Map(setNames, as.list(gmt_collections_paths), names_abbr)
    names(gmt_collections_paths) = test$V2
    gmt_collections_paths = split(gmt_collections_paths,test$V1)
    
    
    ## read in GMTs selected as default
    test = read.csv(paste0(getwd(),"/www/gmts/gmts_list_selected.csv"),header=F,stringsAsFactors = F)
    # names of databases
    dbs = strsplit(test$V3,";")
    for(i in seq_along(dbs)){
        # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
        names <- gsub(".gmt$","",dbs[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
        names_abbr = abbreviate_string(names); coll = names_abbr
        names(coll) = names
        
        # store databases names into the list vector that stores collections
        gmt_collections_selected = c(gmt_collections_selected, list(coll))
    }
    
    # name collections by species names
    names(gmt_collections_selected) = test$V2
    gmt_collections_selected = split(gmt_collections_selected,test$V1)
    
    remove(test); remove(dbs)
}
