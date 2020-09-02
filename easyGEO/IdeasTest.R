location <- "~/Downloads/Taubert/GSE152075_raw_counts_GEO.txt"
res <- try(read.table(location, sep="\t",header=F, colClasses=c("character")))
if(inherits(res, "try-error")) {          #if error happened
  ErrorMessage <- conditionMessage(attr(res, "condition"))  # the error message
}

