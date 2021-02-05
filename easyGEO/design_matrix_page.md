---
title: "Note on uploading design matrix:"
output: html_document
runtime: shiny
---
<!-- rmarkdown::render('design_matrix_page.Rmd', output_file = 'C:/Users/15067/Desktop/eVITTA/easyGEO/server/design_matrix_page.html') -->
### 1) the data should be in comma- or tab-delimited format (csv, tsv, tab, txt),
<br/>
      
### 2) the first row of the matrix should be sample attributes (e.g. strain names, experimental conditions, patient groups); no duplicates are allowed,
<br/>
      
### 3) the first column of the matrix should be sample names; must match the sample names in the data matrix,
<br/>

### For example,
<br/>


|X                         |cell.line |cell.type                                |strain       |subject.status |time.after.treatment |
|:-------------------------|:---------|:----------------------------------------|:------------|:--------------|:--------------------|
|Series1_NHBE_Mock_1       |NHBE      |primary human bronchial epithelial cells |N/A          |N/A            |N/A                  |
|Series1_NHBE_Mock_2       |NHBE      |primary human bronchial epithelial cells |N/A          |N/A            |N/A                  |
|Series1_NHBE_Mock_3       |NHBE      |primary human bronchial epithelial cells |N/A          |N/A            |N/A                  |
|Series1_NHBE_SARS-CoV-2_1 |NHBE      |primary human bronchial epithelial cells |USA-WA1/2020 |N/A            |N/A                  |
|Series1_NHBE_SARS-CoV-2_2 |NHBE      |primary human bronchial epithelial cells |USA-WA1/2020 |N/A            |N/A                  |
|Series1_NHBE_SARS-CoV-2_3 |NHBE      |primary human bronchial epithelial cells |USA-WA1/2020 |N/A            |N/A                  |

