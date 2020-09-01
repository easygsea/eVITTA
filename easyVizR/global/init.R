

# --------------- Presets -------------------

options(spinner.color="#0dc5c1")

# red colorscale
cscale <- list(c(0, "rgb(255, 255, 255)"), # 0 = white
               list(0.200687, "rgb(254,224,144)"), # 0.25 = light yellow
               list(0.333333, "rgb(253,174,97)"), # 0.1 = yellow
               list(0.433677, "rgb(244,109,67)"), # 0.05 = orange
               list(0.666667, "rgb(215,48,39)"), # 0.01 = red
               list(1, "rgb(165,0,38)") # 0.001 = dark red
)
#adapted from colorbrewer blues
cscale_bu2 <- list(c(0, "rgb(255, 255, 255)"), # 0 = white
                   list(0.200687, "rgb(198,219,239)"), # 0.25 = pale blue
                   list(0.333333, "rgb(158,202,225)"), # 0.1 = light blue
                   list(0.433677, "rgb(107,174,214)"), # 0.05 = blue
                   list(0.666667, "rgb(49,130,189)"), # 0.01 = darker blue
                   list(1, "rgb(8,81,156)") # 0.001 = cornflower
)
# -3 to 3 colorscale adapted from cscale + cscale_bu2. Ideal for -log10(p)*sign(logFC).
cscale_div <- list(c(0, "rgb(8,81,156)"), # -0.001 = cornflower,
                   list(0.166667, "rgb(49,130,189)"), # -0.01 = darker blue
                   list(0.283161, "rgb(107,174,214)"), # -0.05 = blue
                   list(0.333333, "rgb(158,202,225)"), # -0.1 = light blue
                   list(0.399656, "rgb(198,219,239)"), # -0.25 = pale blue
                   list(0.5, "rgb(255, 255, 255)"), # 0 = white
                   list(0.600344, "rgb(254,224,144)"), # 0.25 = light yellow
                   list(0.666667, "rgb(253,174,97)"), # 0.1 = yellow
                   list(0.716839, "rgb(244,109,67)"), # 0.05 = orange
                   list(0.833334, "rgb(215,48,39)"), # 0.01 = red
                   list(1, "rgb(165,0,38)") # 0.001 = dark red
)

# -3 to 3 colorscale adapted from cscale + cscale_bu2. evenly distributed.
cscale_simple <- list(c(0, "rgb(8,81,156)"), # -0.001 = cornflower,
                      list(0.1, "rgb(49,130,189)"), # -0.01 = darker blue
                      list(0.2, "rgb(107,174,214)"), # -0.05 = blue
                      list(0.3, "rgb(158,202,225)"), # -0.1 = light blue
                      list(0.4, "rgb(198,219,239)"), # -0.25 = pale blue
                      list(0.5, "rgb(255, 255, 255)"), # 0 = white
                      list(0.6, "rgb(254,224,144)"), # 0.25 = light yellow
                      list(0.7, "rgb(253,174,97)"), # 0.1 = yellow
                      list(0.8, "rgb(244,109,67)"), # 0.05 = orange
                      list(0.9, "rgb(215,48,39)"), # 0.01 = red
                      list(1, "rgb(165,0,38)") # 0.001 = dark red
)

gene_alias <- c("(?i)g(?-i)ene","(?i)n(?-i)ame","(?i)g(?-i)ene.(?i)n(?-i)ame",
                "(?i)S(?-i)ymbol",
                "(?i)p(?-i)athway", "(?i)p(?-i)twy"
)
stat_alias <- c("log2(?i)FC(?-i)","log(?i)fc(?-i)",
                "(?i)e(?-i)nrichment.(?i)s(?-i)core","^(?i)ES(?-i).*",
                "^(?i)S(?-i)tat*"
)
p_alias <- c("(?i)pv(?-i)al.*","(?i)p(?-i).(?i)v(?-i)al.*",
             "^(?i)P(?-i)"
)
q_alias <- c("(?i)qv(?-i)al.*","(?i)q(?-i).(?i)v(?-i)al.*", 
             "^(?i)q(?-i)",
             "(?i)FDR(?-i)", 
             "(?i)p(?-i)adj", 
             "(?i)a(?-i)dj.(?i)p(?-i).(?i)v(?-i)al"
)

loadMsg = "easyVizR"


# --------------- Initialize -------------------

# list filenames
ll <- c("Sample_Hibshman_lite.csv"
        # , "up_hif-1hypoxia_vs_normoxia.csv", "up_N2hypoxia_vs_normoxia.csv", "up_nhr-49hypoxia_vs_normoxia.csv"
        # , "t_efk1starved_vs_fed_vsN2_KEGG.csv", "t_N2starved_vs_fed_KEGG.csv"
        # , "z_efk1starved_vs_fed.csv", "z_N2starved_vs_fed.csv","z_zip2starved_vs_fed.csv"
)
# list paths
pl <- paste0(getwd(),"/inc/", ll, sep = '')
# list dfs
gg <- lapply(pl, read.csv)
# remove any row with na anywhere
gg <- lapply(gg, remove_nas)


ll <- lapply(ll, tidy_filename, list())


tt <- list("logFC"
           # , "logFC", "logFC", "logFC"
           # , "ES", "ES"
           # , "logFC", "logFC"
)




# --------------- Initialize introjs -------------------
intropath <- paste0(getwd(), "/intro/")
filepaths <- list.files(intropath, full.names=T)
intros <- lapply(filepaths, function(x){
  df <- data.frame(read.csv(x, header=T, sep="\t"))
  df$element <- sub("^", "#", df$element)
  df[df=="#"] <- NA
  df
})
names(intros) <- tools::file_path_sans_ext(list.files(intropath, full.names=F))
rownames(intros) <- NULL