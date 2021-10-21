find_min_n <- function(
  samples_c, samples_t
){
  input2rv("count_filter")
  if(rv$count_filter == "default"){
    min_n = min(length(samples_c),length(samples_t))
    # counts in at least rv$min_n samples if too many samples as in scRNAseq
    if(min_n > rv$min_n){min_n = rv$min_n}
  }else if(rv$count_filter == "custom"){
    min_n <- update_numericRV("exp_n")
  }else if(rv$count_filter == "filterByExpr"){
    min_n <- rv$exp_n
  }
  return(min_n)
}

filter_genes <- function(
  y,min_n,raw_or_norm
){
  if(rv$count_filter == "filterByExpr"){
    f1 <- update_numericRV("fbe_min_count")
    f2 <- update_numericRV("fbe_min_total_count")
    f3 <- update_numericRV("fbe_large_n")
    f4 <- update_numericRV("fbe_min_prop")
    keep <- filterByExpr(y,
                         min.count = rv$fbe_min_count, min.total.count = rv$fbe_min_total_count
                         , large.n = rv$fbe_large_n, min.prop = rv$fbe_min_prop
    )
  }
  if(raw_or_norm == "raw"){
    if(rv$count_filter == "default"){
      # filter genes expressed in at least 3 of the samples
      keep <- rowSums(cpm(y)>1) >= min_n
    }else if(rv$count_filter == "custom"){
      expt <- update_numericRV("exp_threshold",min=1)
      if(rv$exp_unit == "cpm"){
        keep <- rowSums(cpm(y)>rv$exp_threshold) >= min_n
      }else if(rv$exp_unit == "counts"){
        keep <- rowSums(y$counts>rv$exp_threshold) >= min_n
      }
    }
  }else{
    if(rv$count_filter == "default"){
      keep <- rowSums(y$counts>1) >= min_n
    }else if(rv$count_filter == "custom"){
      expt <- ifelse_rv_na("exp_threshold"); rv$exp_threshold <- expt
      keep <- rowSums(y$counts>rv$exp_threshold) >= min_n
    }
  }
  return(keep)
}

run_DE <- function(
  m_df, # count matrix
  design1, # design matrix
  min_n, # minimum sample #
  raw_or_norm, # "raw" or "normalized" counts
  p_df, batch_var, c_var, c_level,
  run_method = rv$de_method
){
  if(run_method == "default"){
    y <- run_default(m_df,design1,min_n,raw_or_norm)
  }else if(run_method == "edger"){
    y <- run_edgeR(m_df,design1,min_n,raw_or_norm)
  }else if(run_method == "deseq2"){
    y <- run_DESeq2(m_df,design1,min_n,raw_or_norm
                    ,p_df, batch_var, c_var, c_level)
  }
  return(y)
}

# ----- reactives --------
fdr_column <- function(){
  db <- c("adj.P.Val","FDR","padj")
  cnames <- colnames(rv$deg)
  return(find_overlap(cnames,db))
}

fc_column <- function(){
  db <- c("logFC","log2FoldChange")
  cnames <- colnames(rv$deg)
  return(find_overlap(cnames,db))
}

# ----- DE method: limma (default) --------
run_default <- function(
  m_df, # count matrix
  design1, # design matrix
  min_n, # minimum sample #
  raw_or_norm, # "raw" or "normalized" counts
  edger_norm=rv$edger_norm_method, # method to normalize raw count data, default TMM
  limma_norm=rv$limma_norm_method, # between-array normalization
  limma_fit=rv$limma_fit, # fitting method
  limma_trend=rv$limma_trend, limma_robust=rv$limma_robust
){
  # 3.2) create dgelist
  y <- DGEList(counts=m_df)
  
  # 3.3) filter and normalize if raw read counts, and run limma
  keep <- filter_genes(y,min_n,raw_or_norm)
  y <- y[keep,,keep.lib.sizes=TRUE]
  if(raw_or_norm == "raw"){
    # normalize count data
    y <- calcNormFactors(y, method = edger_norm)
    # voom on normalized data
    v <- voom(y, design1, plot=F)
  }else{
    # # voom directly on counts, if data are very noisy, as would be used for microarray
    v <- voom(y, design1, plot=F, normalize=limma_norm)
  }
  
  # 3.4) DEG analysis
  fit <- lmFit(v, design1, method=limma_fit)
  fit <- eBayes(fit,trend=limma_trend, robust=limma_robust)
  
  # results
  results <- decideTests(fit)
  summary(results)
  
  # export DE table
  degs <- topTable(fit, coef=ncol(fit),sort.by="P",number=Inf)
  rv$deg <- degs
  
  return(y)
}

# ----- DE method: edgeR --------
run_edgeR <- function(
  m_df, # count matrix
  design1, # design matrix
  min_n, # minimum sample #
  raw_or_norm, # "raw" or "normalized" counts
  edger_norm=rv$edger_norm_method, # method to normalize raw count data, default TMM
  trend_method=rv$edger_trend_method, # method for estimating dispersion trend
  test=rv$edger_test # test method
){
  # 3.2) create dgelist
  if(test == "exactTest"){
    y <- DGEList(counts=m_df,group=factor(design1[,2]))
  }else{
    y <- DGEList(counts=m_df)
  }
  
  # 3.3) filter and normalize if raw read counts, and run limma
  keep <- filter_genes(y,min_n,raw_or_norm)
  y <- y[keep,,keep.lib.sizes=TRUE]
  y <- calcNormFactors(y, method = edger_norm)
  
  # estimate dispersions
  y <- estimateDisp(y, trend.method = trend_method)
  if(test == "exactTest"){
    fit <- exactTest(y)
  }else if(test == "glmQLFit"){
    fit <- glmQLFit(y,design1,robust=rv$glmQLFit_robust)
    fit <- glmQLFTest(fit, coef=ncol(fit))
  }
  
  # DE table
  degs <- topTags(fit, sort.by="PValue", n=Inf)$table
  rv$deg <- degs
  
  return(y)
}

# ----- DE method: DESeq2 --------
run_DESeq2 <- function(
  m_df, # count matrix
  design1, # design matrix
  min_n, # minimum sample #
  raw_or_norm, # "raw" or "normalized" counts
  p_df, batch_var, c_var, c_level
){
  if(batch_var != "na"){
    coldata <- p_df[,c(batch_var,c_var)]
    colnames(coldata) <- c("batch","treatment")
    dds <- DESeqDataSetFromMatrix(countData = m_df,colData = coldata,design = ~ batch + treatment)
  }else{
    coldata <- p_df[,c_var,drop=F]
    colnames(coldata) <- c("treatment")
    dds <- DESeqDataSetFromMatrix(countData = m_df,colData = coldata,design = ~ treatment)
  }
  keep <- filter_genes(dds,min_n,raw_or_norm)
  dds <- dds[keep,]
  dds$treatment <- relevel(dds$treatment, ref = c_level)
  if(rv$deseq2_test == "LRT"){
    if(batch_var != "na"){
      dds <- DESeq(dds,test = rv$deseq2_test, reduced = ~batch
                   , fitType = rv$deseq2_fit, sfType = rv$deseq2_sf, betaPrior = rv$deseq2_bp)
    }else{
      dds <- DESeq(dds,test = rv$deseq2_test, reduced = ~1
                   , fitType = rv$deseq2_fit, sfType = rv$deseq2_sf, betaPrior = rv$deseq2_bp)
    }
  }else{
    dds <- DESeq(dds,test = rv$deseq2_test, fitType = rv$deseq2_fit, sfType = rv$deseq2_sf, betaPrior = rv$deseq2_bp)
  }
  ress <- resultsNames(dds)
  if(rv$deseq2_shrink != "none"){
    res <- lfcShrink(dds, coef=ress[length(ress)], type=rv$deseq2_shrink)
  }else{
    res <- results(dds)
  }
  rv$deg <- as.data.frame(res) %>% arrange(pvalue)
  return(dds)
}
