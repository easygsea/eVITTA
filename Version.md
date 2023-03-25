# eVITTA update log

### V1.4.4

March 24, 2023 easyGSEA added widgets to download visNetwork input data (Jean)

- Download buttons added to export nodes and edges (if applicable) tables used to plot the visNetwork in Enrichment Network

### V1.4.3

July 26, 2022 easyGEO fixed connection size error for larger GSE datasets (Judith)

- If default connection size doesn't work, try to load with a larger connection size

### V1.4.2

July 14, 2022 easyVizR upload dataframe filtering for blank/ Inf values (Judith)

- Filters rows with blank or Inf values in the required columns

### V1.4.1

June 20, 2022 added easyGEO GSE size gater (Jean)
- auto-detect if a GSE file to download exceeds the maximum allowance (default: 10GB)
- render a modal UI with a link to direct the user to download from NCBI official website

### V1.4.0

June 10, 2022 added easyVizR correlogram module (Parsa/ Judith)

- static and interactive versions of correlation heatmap and correlogram
- soft and hard limits for rendering correlogram based on data size and plot type

### V1.3.2

Nov 03, 2021 easyGEO & easyGSEA function improvement (Jean)

* Volcano log transformation specified as log10 & geom_text_repel max.overlaps set to 20
* easyGSEA added "...1" as a default gene column due to update in read_delim

### V1.3.1

Oct 21, 2021 easyGEO run button req fix (Jean)

### V1.3.0

Oct 21, 2021 Constructed customizable gene filtering, and adjustable DE analysis pipelines with limma, edgeR, and DESeq2 (Jean)

- Three methods to filter lowly expressed genes: default, as described in the manuscript; filterByExpr by edgeR; and custom, where the user defines the expression unit, the number of expression units, and the number of samples that pass the expression threshold criteria.
- limma (default): applies to both raw and normalized counts, as well as array data. Allows normalization on both raw counts (edgeR TMM (default), TMMwsp, RLE, upperquantile) and normalized/array data (limma quantile (default), scale, cyclicloess). Adjustable fitting methods (ls, robust) and fitting parameter refinement (fitting-trend, robosified against outlier sample variances).
- edgeR: applies to raw counts only. Allows raw count normalization (TMM (default), TMMwsp, RLE, upperquantile); dispersion trend estimation (locfit (default), movingage, loess, locfit.mixed); DE test with both glmQLFit (w/ or w/o robust prior QL estimation) and exactTest.
- DESeq2: applies to raw counts only. Allows DE test with both Wald (default) and LRT methods; adjustable fit types (parametric, local, mean, glmGamPoi); size factor estimation (ratio, poscounts, iterate); zero-mean normal prior application (betaPrior); and log2 fold change shrinkage (apeglm, ashr, normal).

### V1.2.14

Sep 2, 2021 Revised auto-installation of Bioconductor packages in all global files (Jean)

### V1.2.13

Aug 18, 2021 Added color scales to easyGEO's heatmap (Judith)

- Added a variety of divergent and sequential colorscales, including colorblind-friendly options
- Add auto install of scales package to easyVizR

### V1.2.12

July 30, 2021 Added support for parsing two-channel microarray data in easyGEO (Judith)

- Changed parsing method for two-channel microarray data
- Data matrix and design matrix for two-channel microarrays can now be displayed in easyGEO

### V1.2.11

July 20, 2021 Non-numeric data check in data matrix (Jean)

- Added a function to check non-numeric data in data matrix
- Rendered a warning and a reminder to users

### V1.2.10

July 6, 2021 Improved customization for easyVizR heatmap (Judith)

- Added a variety of divergent and sequential colorscales, including colorblind-friendly options
- Fixed bug causing heatmap not to render in example run when user chooses to show Y axis labels

### V1.2.9

Jun 27, 2021 Garbage collection on session close (Jean)

- Added a function to clean memory use upon session close for better user experience

### V1.2.8
May 23, 2021 easyGEO microarray negative counts error (Jean)
- If negative values detected in gene expression table, error reminder message rendered to user

### V1.2.7

May 1, 2021 Database extraction from https (Jean)
- GMT libraries and ORA genomic backgrounds accessed via https directly in easyGSEA

### V1.2.6

Apr 22, 2021 Workflow optimization (Jean)
- Added set.seed to create simulated values that are reproducible before running fgsea
- Wrapped waiting messages into wait_msg function if data processing takes to long
- ORA gene name toupper conversion for gconvert results

***

### V1.0.1

Mar 16, 2021 Bug fixed (Blake)
- pictures of our apps on our homepage changed
- names of pictures changed from uppercases to all lowercases
- easyGEO the time that the introjs appear is adjusted to 1 second later
- gsea help page list item position adjusted

Mar 18, 2021 Homepage update and download button optimization (Jean)
- Updated easyGEO & easyGSEA help pages
- easyGSEA tab 2 enrichment plot download dropdown bug fix

### V1.0.2

Mar 23, 2021 Function improvement (Jean)
- easyGSEA ORA module check_numericInput_na improved

### V1.0.3

Mar 23, 2021 UI update and ORA module optimization (Jean)
- easyGSEA database selection changed from selectizeInput to pickerInput
- easyGSEA ORA module sig_none reactive updated
- easyGSEA ORA bubble check_numericInput_na to catch none input
- easyGSEA ORA bar & bubble manual GS selection UI optimization
- easyGSEA ORA bar & bubble arranged from low-to-high pval

### V1.0.4

March 24, 2021 workflow improvement (Jean)
- easyGSEA GSEA run padj automatic filtering adjusted to assess for min of up/down

## V1.1.0

March 24, 2021 easyGEO visualization update (Jean)
- easyGEO volcano plot updated with options to toggle line plotting

### V1.1.1

March 24, 2021 easyGSEA visualization update (Jean)
- KEGG plot gene.idtype parameter assigned with a new reactive variable gidtype (to handle issues with yeast SGD database)
- Added rno.rnk and rno.csv

### V1.1.2

March 24, 2021 Google Analytics added (Jean)

## V1.2.0

March 25, 2021 easyGSEA ORA reference added with genome background option (Jean)

### V1.2.1
March 30, 2021 Homepage updated with easyGEO tutorial video (Blake)

### V1.2.2

March 30, 2021 VizR text fixes (Judith):

- changed "gene list" to "filtered list"
- added "BP" to ignored strings in wordcloud
- added dataset names to RRHO level plot X and Y axes

March 31, 2021 GEO & VizR free RAM check bug fix (Jean)

### V1.2.3

April 2, 2021 (Jean)
- GSEA ORA gene list input comma clean up and limit set to 5000 genes at maximum
- Cleaned up shinyapps.io folders

April 3, 2021 (Jean)
- Deleted basic_scripts and profvis_tests folders
- Tidies easyGEO server.R, functions.R scripts
- Deleted desktop.ini
- Deleted intro_demo folder and its corresponding script in global.R

### V1.2.4

April 6, 2021 (Blake)
- Deleted easyGEO files that are unused
- clean up some files and move some files into inc folder
- Deleted some ununsed images and html pages in the homepage folder
- updated the download data button in easyGEO, optimized it to download files with sample names that are easy to read

April 8, 2021 (Jean)
- Revised README.md: Introduction of eVITTA modules; gene set database download intruction guide; quick start guide

April 8, 2021 (Judith)

- Cleaned up easyVizR unused files and commented out print lines
- Suppress warning msgs while initializing intro tour text for easyVizR
- VizR rank scatter add p val testing and display

### V1.2.5

April 9, 2021 (Jean)
- Gathered essential files and scripts for code release
