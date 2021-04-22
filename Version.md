# eVITTA update log

### V1.2.7

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

