col_f = c("01_WormCat (Holdorf et al. 2020)","02_Pathway","03_Gene Ontology")

# databases has prefices
db_prs <- c("KEGG","RAL","RA","WP","GO","BP","CC","MF","C1","C2","C3","HALLMARK")

# width of shadows
shadow_width <- ".3em"
line_width <- ".5px"

# p_min to convert p=0
p_min = 1e-300

# slider cutoffs for p/q
cutoff_slider = c(0.0001,0.0005,0.001,0.005,0.01,0.025,0.05,0.075,0.1,0.25,0.3,0.5,1)

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

src <- reactive({
  if(rv$demo_mode == "gsea" && rv$es_term == "WP_Type_I_Interferon_Induction_and_Signaling_During_SARS-CoV-2_Infection%WP4868"){
    src <- NULL
  }else if(rv$demo_mode == "ora" && rv$es_term == "RA_Transcriptional_activity_of_SMAD2/SMAD3:SMAD4_heterotrimer%R-CEL-2173793"){
    src <- NULL
  }else{
    src <- src_pathway
  }
  return(src)
})

# ===================== COLOR SCALES for visualizations =====================
# color bar for GSEA output visualizations (blue and red) - ggolot version
gcols = c(rgb(8,81,156,maxColorValue = 255), # -0.001 = cornflower,
          rgb(9,91,175,maxColorValue = 255), # -0.01 = darker blue 49,130,189
          rgb(10,100,193,maxColorValue = 255), # -0.05 = blue 107,174,214
          rgb(11,110,212,maxColorValue = 255), # -0.1 = light blue 158,202,225
          rgb(12,120,231,maxColorValue = 255), # -0.25 = pale blue 198,219,239
          rgb(255, 255, 255,maxColorValue = 255), # 0 = white
          rgb(243,0,56,maxColorValue = 255), # 0.25 = light yellow 254,224,144
          rgb(224,0,52,maxColorValue = 255), # 0.1 = yellow 253,174,97
          rgb(204,0,47,maxColorValue = 255), # 0.05 = orange 244,109,67
          rgb(185,0,43,maxColorValue = 255), # 0.01 = red 215,48,39
          rgb(165,0,38,maxColorValue = 255)) # 0.001 = dark red

# value scales for GSEA output visualizations (blue and red) - ggolot version
gvalues = rescale(c(-3,-2,log10(0.05),-1,log10(0.25),0,-log10(0.25),1,-log10(0.05),2,3))

# color bar for ORA output visualizations (red only) - ggolot version
gcols_red = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
           rgb(243,0,56,maxColorValue = 255), # 0.25 = light yellow 254,224,144
           rgb(224,0,52,maxColorValue = 255), # 0.1 = yellow 253,174,97
           rgb(204,0,47,maxColorValue = 255), # 0.05 = orange 244,109,67
           rgb(185,0,43,maxColorValue = 255), # 0.01 = red 215,48,39
           rgb(165,0,38,maxColorValue = 255)) # 0.001 = dark red

gcols_red_vis <- c(
  "rgba(254,224,144)","rgba(253,174,97)","rgba(244,109,67)","rgba(215,48,39)","rgba(165,0,38)"
)

# color bar for ORA output visualizations (blue only) - ggolot version
gcols_blue = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
           rgb(12,120,231,maxColorValue = 255),
           rgb(11,110,212,maxColorValue = 255),
           rgb(10,100,193,maxColorValue = 255),
           rgb(9,91,175,maxColorValue = 255),
           rgb(8,81,156,maxColorValue = 255)) # 0.001 = cornflower

gcols_blue_vis <- c(
  "rgba(198,219,239)","rgba(158,202,225)","rgba(107,174,214)","rgba(49,130,189)","rgba(8,81,156)"
)

# color bar for ORA output visualizations (salmon only) - ggolot version
gcols_salmon = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
               rgb(252,187,182,maxColorValue = 255),
               rgb(250,164,158,maxColorValue = 255),
               rgb(249,141,133,maxColorValue = 255),
               rgb(248,118,109,maxColorValue = 255), # 0.005 = salmon
               rgb(247,95,85,maxColorValue = 255)) # 0.001

gcols_salmon_vis <- c(
  "rgba(252,187,182)","rgba(250,164,158)","rgba(249,141,133)","rgba(248,118,109)","rgba(247,95,85)"
)

# color bar for ORA output visualizations (cyan only) - ggolot version
gcols_cyan <- c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
               rgb(77,195,255,maxColorValue = 255),
               rgb(51,186,255,maxColorValue = 255),
               rgb(26,178,255,maxColorValue = 255),
               rgb(0,169,255,maxColorValue = 255), # 0.005 = cyan
               rgb(0,152,230,maxColorValue = 255)) # 0.001

gcols_cyan_vis <- c(
  "rgba(77,195,255)","rgba(51,186,255)","rgba(26,178,255)","rgba(0,169,255)","rgba(0,152,230)"
)

# color bar for ORA output visualizations (orange only) - ggolot version
gcols_orange = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                 rgb(255,192,77,maxColorValue = 255), # 0.25 = 
                 rgb(255,183,51,maxColorValue = 255), # 0.05 = 
                 rgb(255,174,26,maxColorValue = 255), # 0.01 = 
                 rgb(255,165,0,maxColorValue = 255), # 0.005 = orange
                 rgb(230,149,0,maxColorValue = 255)) # 0.001 = 

gcols_orange_vis <- c(
  "rgba(255,192,77)","rgba(255,183,51)","rgba(255,174,26)","rgba(255,165,0)","rgba(230,149,0)" #
)

# color bar for ORA output visualizations (green only) - ggolot version
gcols_green = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                rgb(183,238,0,maxColorValue = 255), # 0.25 = 
                rgb(163,212,0,maxColorValue = 255), # 0.05 = 
                rgb(144,187,0,maxColorValue = 255), # 0.01 = 
                rgb(124,161,0,maxColorValue = 255), # 0.005 = 
                rgb(104,136,0,maxColorValue = 255)) # 0.001 = green

gcols_green_vis <- c(
  "rgba(255,192,77)","rgba(183,238,0)","rgba(144,187,0)","rgba(124,161,0)","rgba(104,136,0)"
)

# color bar for ORA output visualizations (purple only) - ggolot version
gcols_purple = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                 rgb(232,201,255,maxColorValue = 255), # 0.25 = lavender
                 rgb(221,175,255,maxColorValue = 255), # 0.05 = thistle
                 rgb(210,150,255,maxColorValue = 255), # 0.01 = plum
                 rgb(199,124,255,maxColorValue = 255), # 0.005 = orchid
                 rgb(188,99,255,maxColorValue = 255)) # 0.001 = mediumorchid

gcols_purple_vis <- c(
  "rgba(232,201,255)","rgba(221,175,255)","rgba(210,150,255)","rgba(199,124,255)","rgba(188,99,255)"
)

# color bar for ORA output visualizations (grey only) - ggolot version
gcols_grey = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
               rgb(220,220,220,maxColorValue = 255), # 0.25 = gainsboro
               rgb(192,192,192,maxColorValue = 255), # 0.05 = silver
               rgb(169,169,169,maxColorValue = 255), # 0.01 = darkgrey
               rgb(128,128,128,maxColorValue = 255), # 0.005 = grey
               rgb(105,105,105,maxColorValue = 255)) # 0.001 = dimgrey

gcols_grey_vis <- c(
  "rgba(220,220,220)","rgba(192,192,192)","rgba(169,169,169)","rgba(128,128,128)","rgba(105,105,105)"
)

# value scales for ORA output visualizations (red only) - ggolot version
gvalues2 = rescale(c(0,-log10(0.25),1,-log10(0.05),2,3))


#===================== GMT collections' names =====================
gmt_abbr <- read_tsv(paste0(getwd(),"/www/gmt_abbreviations.tsv"),col_names = F)
gmt_names <- as.list(gmt_abbr$X2)
names(gmt_names) <- gmt_abbr$X1

# function to retrieve abbreviations
retrieve_abbr <- function(name){
  name <- str_split(name, "-(?=[^-]+$)")[[1]][1]
  gmt_names[grepl(paste0("^",name),gmt_names,ignore.case = T)][1] %>% names(.)
}


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
dbs_o = strsplit(test$V3,";")
for(i in seq_along(dbs_o)){
  # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
  names <- gsub(".gmt$","",dbs_o[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
  # # 1) the below 2 lines store the databases in abbreviated strings and named them in full in a named vector
  names_abbr = sapply(names, function(x) retrieve_abbr(x));  coll = names_abbr #names_abbr = abbreviate_string(names); coll = names_abbr
  names(coll) = names
  # # 2) the below 1 line stores the databases' names in full
  # coll = names
  
  # store databases names into the list vector that stores collections
  gmt_collections = c(gmt_collections, list(coll))
  
  # paths to GMT files
  paths = paste0(getwd(),"/www/gmts/",test$V1[[i]],"/",test$V2[[i]],"/",dbs_o[[i]])
  names(paths) = names_abbr #names
  
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
dbs_o = strsplit(test$V3,";")
for(i in seq_along(dbs_o)){
  # tidy up database names by removing ".gmt" and the dates it's created; convert _ to spaces
  names <- gsub(".gmt$","",dbs_o[[i]]);names <- gsub("\\d\\d+$","",names);names <- gsub("_"," ",names)
  # # 1) the below 2 lines store the databases in abbreviated strings and named them in full in a named vector
  names_abbr = sapply(names, function(x) retrieve_abbr(x)); coll = names_abbr #names_abbr = abbreviate_string(names); coll = names_abbr
  names(coll) = names
  # # 2) the below 1 line stores the databases' names in full
  # coll = names
  
  # store databases names into the list vector that stores collections
  gmt_collections_selected = c(gmt_collections_selected, list(coll))
}

# name collections by species names
names(gmt_collections_selected) = test$V2
gmt_collections_selected = split(gmt_collections_selected,test$V1)

remove(test); remove(dbs_o)


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

#============ add help annotations ==============
db_bs <- "Select or de-select the functional database(s) for your run"
p_bs <- "Gene sets with a P-value &lt; the selected threshold will be kept. Drag the slider to change the threshold"
q_bs <- "Gene sets with an adjusted P-value &lt; the selected threshold will be kept. Drag the slider to change the threshold"
# pq bsToopTip annotation for manhattan or volcano
man_pq_c_bs <- "Gene sets with a P or adjusted P-value &lt; the selected threshold will be highlighted with darker colors. Drag the slider to change the threshold"
pq_bs <- "Choose P-value or adjusted P-value to color the plot"
abb_bs <- "Abbreviate the labels when the texts are too long to be displayed. <b>Yes</b> will extract the first characters, as defined by <b>String length</b>, for display"
len_bs <- "Number of characters for the abbreviated y-axis labels"
up_bs <- "Number of top upregulated gene set(s)"
down_bs <- "Number of top downregulated gene set(s)"
oratop_bs <- "Number of top enriched gene set(s)"
manual_bs <- "Manually search, select and plot the gene set(s) of interest"
bubble_size_bs <- "Drag the slider to adjust the maximum and minimum bubble sizes"
vol_mode_bs <- "<b>Continuous</b> and <b>Discrete</b> are hoverable, clickable and interactive. <b>Static</b> labels the top regulations with texts."
col_tone_bs <- "Scroll down and click to select the color tone"
db_bs <- "By default, each gene set is prefixed by its originating database, i.e. the identifier. Unselect to delete the identifier."
id_bs <- "By defualt, each gene set is annotated with its unique ID (if any) in the original database. Unselect to delete the ID string."
edge_bs <- "Reference data to calculate gene set similarities"
