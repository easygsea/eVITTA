#======================================================================#
####                          FUNCTIONS                             ####
#======================================================================#

# preview a palette on a horizontal discrete scale
#------------------------------------------------
previewPalette <- function(v1){
  testName=deparse(substitute(v1))
  testPal= v1
  n=length(testPal)
  image(1:n,1,as.matrix(1:n),col=testPal,
        xlab=paste0(testName," (n=",n,")"),ylab="",xaxt="n",yaxt="n",bty="n")
}

# preview a palette using scales library
#------------------------------------------------
previewPaletteScales <- function(v1){
  show_col(v1)
}


# combine two sequential palettes into one divergent palette
#-----------------------------------------------------------
# neg_pal: palette for negative side
# pos_pal: palette for positive side
# flip_neg: whether to flip the negative palette

combineSeqPalettes <- function(neg_pal, pos_pal, flip_neg=T){
  if (flip_neg==T){ neg_pal <- rev(neg_pal) }
  # detect if the middle is the same color repeated, if so, only leave one copy
  if (neg_pal[[length(neg_pal)]]==pos_pal[[1]]){
    neg_pal <- neg_pal[1:length(neg_pal)-1]
  }
  c(neg_pal, pos_pal)
}



# convert a rgb color palette to plotly color scale
#------------------------------------------------
# palette: in the format of a list of rgb objects
# scale: provide the variable name of the numeric scale in string, e.g. "divLogScale1"

makePlotlyColorscale <- function(palette, scale="divLogScale1", rev=F){
  getScale <- try(eval(as.name(scale)),silent = TRUE)
  if (rev==T){palette<-rev(palette)}
  
  # automatically look for a numeric scale with matching # of steps
  if (class(getScale) != "try-error" & length(palette)==length(getScale)){
    print(paste0("creating plotly palette with specified scale ",scale," (",length(getScale)," steps)..."))
    useScale <- getScale
  } else { # if not found, default to even scale
    warning(paste0("creating plotly palette with default even scale, because cannot find specified scale (",scale,") with matching number of steps (",(length(palette)),")."))
    useScale <- rescale( seq(1, length(palette)) )
  }
  
  # assemble the list of rgb values
  out <- vector(mode="list", length=length(palette))
  for(i in 1:length(out)){
    rval=col2rgb(palette[[i]])[[1]]
    gval=col2rgb(palette[[i]])[[2]]
    bval=col2rgb(palette[[i]])[[3]]
    out[[i]] <- list(useScale[[i]], 
                     paste0("rgb(",
                            rval,",", 
                            gval,",", 
                            bval,")")
    )
  }
  out
}

# df summary of all available palettes, their length, type and display name
#--------------------------------------------------------------
getAllPalettes <- function(list){
  allNames <- unlist(lapply(list, function(x){get(x)}))
  allLengths <- unlist(lapply(allNames, function(x){length(get(x))}))
  allTypes <- unlist(lapply(list, function(x){rep(x, length(get(x)))}))
  allDisplayTypes<- allTypes
  allDisplayTypes <- replace(allDisplayTypes, allTypes=="seqScalesListORA", "Custom sequential")
  allDisplayTypes <- replace(allDisplayTypes, allTypes=="seqScalesList", "Sequential")
  allDisplayTypes <- replace(allDisplayTypes, allTypes=="divScalesList", "Divergent")
  
  allDisplayNames <- paste0(allNames
                            # , " (", allLengths," steps)"
                            )
  
  data.frame(
    name = allNames,
    length = allLengths,
    collection = allTypes,
    type = allDisplayTypes,
    display = allDisplayNames
  )
}

# generate choices for selected palette sets
#-------------------------------------------------------------
generatePaletteChoices <- function(list, allPalettes=allPalettes){
  out <- lapply(list, function(x){
    subset <- allPalettes[allPalettes$type==x,]
    display <- subset$name
    names(display) <- subset$display
    display
  })
  names(out) <- list
  out
  
}

#======================================================================#
####                      CONSTRUCT COLORSCALE                      ####
#======================================================================#

# a colorscale is made of color palette + corresponding numeric scale
# FOR an uneven plotly colorscale (i.e. color levels fixed at numeric %):
# testScale <- makePlotlyColorscale(rev(YlOrRd), "negLogScale1")
# FOR an even plotly colorscale (i.e. color levels evenly spaced):
# testScale <- makePlotlyColorscale(YlOrRd, "even")

# ---------------- numeric scales ---------------------
# these should be paired with a color palette when use

# list of numeric scales
steps6 <- c("even","posLogScale1","negLogScale1")
steps11 <- c("even","divLogScale1")

# divergent log scale 1 (11 steps, -3 to 3)
divLogScale1 <- rescale(c(
  log10(0.001), log10(0.01), log10(0.05), log10(0.1), log10(0.25),
  0,
  -log10(0.25), -log10(0.1), -log10(0.05), -log10(0.01), -log10(0.001)
))

# pos log scale 1 (6 steps, 0 to 3)
posLogScale1 <- rescale(c(
  0,
  -log10(0.25), -log10(0.1), -log10(0.05), -log10(0.01), -log10(0.001)
))

# neg log scale 1 (6 steps, -3 to 3)
negLogScale1 <- rescale(c(
  log10(0.001), log10(0.01), log10(0.05), log10(0.1), log10(0.25), 
  0
))

# divergent log scale 2 (11 steps, -3 to 3)
divLogScale2 <- rescale(c(
  -3, -2, log10(0.05), -1, log10(0.25),
  0,
  -log10(0.25), -1, -log10(0.05), 2, 3
))


# ---------------- sequential color palettes (easyGSEA ORA ggplot) ---------------------
# found in easyGSEA/server/server-variables.R
# (all are 6 steps, starting with white)

# list of Sequential color scales for ggplot ORA visualizations
seqScalesListORA <- c("gcols_red","gcols_blue","gcols_salmon", "gcols_cyan", "gcols_orange", "gcols_green", "gcols_purple", "gcols_grey")


# color bar for ORA output visualizations (red only) - ggolot version
gcols_red = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
              rgb(243,0,56,maxColorValue = 255), # 0.25 = light yellow 254,224,144
              rgb(224,0,52,maxColorValue = 255), # 0.1 = yellow 253,174,97
              rgb(204,0,47,maxColorValue = 255), # 0.05 = orange 244,109,67
              rgb(185,0,43,maxColorValue = 255), # 0.01 = red 215,48,39
              rgb(165,0,38,maxColorValue = 255)) # 0.001 = dark red

# color bar for ORA output visualizations (blue only) - ggolot version
gcols_blue = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
               rgb(12,120,231,maxColorValue = 255),
               rgb(11,110,212,maxColorValue = 255),
               rgb(10,100,193,maxColorValue = 255),
               rgb(9,91,175,maxColorValue = 255),
               rgb(8,81,156,maxColorValue = 255)) # 0.001 = cornflower

# color bar for ORA output visualizations (salmon only) - ggolot version
gcols_salmon = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                 rgb(252,187,182,maxColorValue = 255),
                 rgb(250,164,158,maxColorValue = 255),
                 rgb(249,141,133,maxColorValue = 255),
                 rgb(248,118,109,maxColorValue = 255), # 0.005 = salmon
                 rgb(247,95,85,maxColorValue = 255)) # 0.001

# color bar for ORA output visualizations (cyan only) - ggolot version
gcols_cyan <- c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                rgb(77,195,255,maxColorValue = 255),
                rgb(51,186,255,maxColorValue = 255),
                rgb(26,178,255,maxColorValue = 255),
                rgb(0,169,255,maxColorValue = 255), # 0.005 = cyan
                rgb(0,152,230,maxColorValue = 255)) # 0.001

# color bar for ORA output visualizations (orange only) - ggolot version
gcols_orange = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                 rgb(255,192,77,maxColorValue = 255), # 0.25 = 
                 rgb(255,183,51,maxColorValue = 255), # 0.05 = 
                 rgb(255,174,26,maxColorValue = 255), # 0.01 = 
                 rgb(255,165,0,maxColorValue = 255), # 0.005 = orange
                 rgb(230,149,0,maxColorValue = 255)) # 0.001 = 

# color bar for ORA output visualizations (green only) - ggolot version
gcols_green = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                rgb(183,238,0,maxColorValue = 255), # 0.25 = 
                rgb(163,212,0,maxColorValue = 255), # 0.05 = 
                rgb(144,187,0,maxColorValue = 255), # 0.01 = 
                rgb(124,161,0,maxColorValue = 255), # 0.005 = 
                rgb(104,136,0,maxColorValue = 255)) # 0.001 = green

# color bar for ORA output visualizations (purple only) - ggolot version
gcols_purple = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
                 rgb(232,201,255,maxColorValue = 255), # 0.25 = lavender
                 rgb(221,175,255,maxColorValue = 255), # 0.05 = thistle
                 rgb(210,150,255,maxColorValue = 255), # 0.01 = plum
                 rgb(199,124,255,maxColorValue = 255), # 0.005 = orchid
                 rgb(188,99,255,maxColorValue = 255)) # 0.001 = mediumorchid

# color bar for ORA output visualizations (grey only) - ggolot version
gcols_grey = c(rgb(255, 255, 255,maxColorValue = 255), # 0 = white
               rgb(220,220,220,maxColorValue = 255), # 0.25 = gainsboro
               rgb(192,192,192,maxColorValue = 255), # 0.05 = silver
               rgb(169,169,169,maxColorValue = 255), # 0.01 = darkgrey
               rgb(128,128,128,maxColorValue = 255), # 0.005 = grey
               rgb(105,105,105,maxColorValue = 255)) # 0.001 = dimgrey


# ---------------- Sequential color palettes ---------------------
# found in easyVizR/global/init.R

# list of Sequential color scales
seqScalesList <- c("yellowRed", "blueCornflower", "YlOrRd", "YlOrBr" , "YlGnBu", "RdPu", "Purples", "PuRd", "PuBuGn", "PuBu", "OrRd", "Oranges","Greys", "Greens", "GnBu", "BuPu", "BuGn", "Blues")

# yellow red color palette (6 steps, 0 to 3)
# recommended posLogScale1
yellowRed <- c(
  rgb(255,255,255,maxColorValue = 255), # 0 = white
  rgb(254,224,144,maxColorValue = 255), # 0.25 = light yellow
  rgb(253,174,97,maxColorValue = 255), # 0.1 = yellow
  rgb(244,109,67,maxColorValue = 255), # 0.05 = orange
  rgb(215,48,39,maxColorValue = 255), # 0.01 = red
  rgb(165,0,38,maxColorValue = 255) # 0.001 = dark red
)

# blue gradient color palette (6 steps)
# recommended posLogScale1 (0 to 3), or its rev with negLogScale1 (-3 to 0)
blueCornflower <- c(
  rgb(255,255,255,maxColorValue = 255), # 0 = white
  rgb(198,219,239,maxColorValue = 255), # -0.25 = pale blue
  rgb(158,202,225,maxColorValue = 255), # -0.1 = light blue
  rgb(107,174,214,maxColorValue = 255), # -0.05 = blue
  rgb(49,130,189,maxColorValue = 255), # -0.01 = darker blue
  rgb(8,81,156,maxColorValue = 255) # -0.001 = cornflower
)

# more sequential palettes
YlOrRd<-brewer.pal(6,"YlOrRd")
YlOrBr<-brewer.pal(6,"YlOrBr")
YlGnBu<-brewer.pal(6,"YlGnBu")
RdPu<-brewer.pal(6,"RdPu")
Purples<-brewer.pal(6,"Purples")
PuRd<-brewer.pal(6,"PuRd")
PuBuGn<-brewer.pal(6,"PuBuGn")
PuBu<-brewer.pal(6,"PuBu")
OrRd<-brewer.pal(6,"OrRd")
Oranges<-brewer.pal(6,"Oranges")
Greys<-brewer.pal(6,"Greys")
Greens<-brewer.pal(6,"Greens")
GnBu<-brewer.pal(6,"GnBu")
BuPu<-brewer.pal(6,"BuPu")
BuGn<-brewer.pal(6,"BuGn")
Blues<-brewer.pal(6,"Blues")


# ---------------- Divergent color palettes ---------------------

# List of Divergent color scales
divScalesList <- c("redBlueDiv",
                   "RdYlBu",
                   "RdBu",
                   "PuOr",
                   "PRGn",
                   "BrBG"
)

# red blue divergent color palette (11 steps, -3 to 3) 
# recommended divLogScale1 or divLogScale2
redBlueDiv <- c(
  rgb(8,81,156,maxColorValue = 255), # -0.001 = cornflower,
  rgb(49,130,189,maxColorValue = 255), # -0.01 = darker blue
  rgb(107,174,214,maxColorValue = 255), # -0.05 = blue
  rgb(158,202,225,maxColorValue = 255), # -0.1 = light blue
  rgb(198,219,239,maxColorValue = 255), # -0.25 = pale blue
  rgb(255,255,255,maxColorValue = 255), # 0 = white
  rgb(254,224,144,maxColorValue = 255), # 0.25 = light yellow
  rgb(253,174,97,maxColorValue = 255), # 0.1 = yellow
  rgb(244,109,67,maxColorValue = 255), # 0.05 = orange
  rgb(215,48,39,maxColorValue = 255), # 0.01 = red
  rgb(165,0,38,maxColorValue = 255) # 0.001 = dark red
)
# this is identical to combineSeqPalettes(blueCornflower,yellowRed)

# built in color scales
RdYlBu<-rev(brewer.pal(11,"RdYlBu"))
RdBu<-rev(brewer.pal(11,"RdBu"))
PuOr<-rev(brewer.pal(11,"PuOr"))
PRGn<-rev(brewer.pal(11,"PRGn"))
BrBG<-rev(brewer.pal(11,"BrBG"))



#======================================================================#
####                      INITIALIZE PALETTES                       ####
#======================================================================#

# list all palettes info in dataframe
allPalettes <- getAllPalettes(list("seqScalesListORA","seqScalesList","divScalesList"))

# generate choices
seq_div_colorChoices <- generatePaletteChoices(c("Sequential","Divergent"), allPalettes)
div_seq_colorChoices <- generatePaletteChoices(c("Divergent","Sequential"), allPalettes)
div_seq_ora_colorChoices <- generatePaletteChoices(c("Divergent","Sequential", "Custom sequential"), allPalettes)


