col_f = c("01_WormCat (Holdorf et al. 2020)","02_Pathway","03_Gene Ontology")

# p_min to convert p=0
p_min = 1e-300

# slider cutoffs for p/q
cutoff_slider = c(0.0001,0.0005,0.001,0.005,0.01,0.05,0.075,0.1,0.25,0.3,0.5,1)

# stop words and words with minimum meaning to be filtered for word counts
data(stop_words)
useless_words <- read_csv(paste0(getwd(),"/inc/some_words.csv"))

# wellpanel background colors
bcol1 = "#e6f4fc" # color for parameters wellpanel
bcol2 = "#ffffe6" # color for summary box
bcol3 = "#e6f4fc" # color for column selection wellpanel

# script to scroll down directly
src_pathway <- tags$script(HTML(
  "document.getElementById('kegg_reactome_wp').scrollIntoView();"
))

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

#============= numeric namespaces ================
num_space <- list(
  "bta" = c("ENTREZGENE_ACC","WIKIGENE_ACC")
  ,"cfa" = c("ENTREZGENE_ACC","WIKIGENE_ACC")
  ,"cel" = c("AFFY_GPL19230","ENTREZGENE_ACC","WIKIGENE_ACC")
  ,"dre" = c("AFFY_ZEBGENE_1_0_ST_V1","AFFY_ZEBGENE_1_1_ST_V1","ENTREZGENE_ACC","VEGA_TRANSLATION","WIKIGENE_ACC")
  ,"drm" = c("ENTREZGENE")
  ,"gga" = c("ENTREZGENE_ACC","WIKIGENE_ACC")
  ,"hsa" = c("AFFY_HUEX_1_0_ST_V2","AFFY_HUGENE_1_0_ST_V1","AFFY_HUGENE_2_0_ST_V1","DBASS3_ACC","DBASS5_ACC","ENTREZGENE_ACC","ILLUMINA_HUMANWG_6_V1","MIM_GENE_ACC","MIM_MORBID_ACC","WIKIGENE_ACC")
  ,"mmu" = c("AFFY_MOEX_1_0_ST_V1","AFFY_MOGENE_1_0_ST_V1","AFFY_MOGENE_2_1_ST_V1","ENTREZGENE_ACC","WIKIGENE_ACC")
  ,"sce" = c("ENTREZGENE_ACC")
  ,"ssc" = c("ENTREZGENE_ACC")
)
