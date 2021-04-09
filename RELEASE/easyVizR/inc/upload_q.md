---
title: "Edge parameters: Determine degree of gene overlap"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---

EasyVizR requires four essential columns in your data:

- **Name**: gene names or pathway names
- **Stat**: the main statistic that describes expression; e.g. logFC for RNA-seq data, and ES for GSEA data.
- **PValue**
- **FDR**: q-value. Adjusted p value can also be used.

Column names *do not* need to match these terms exactly. You will be asked to specify which columns are which after upload.

<br/>

#### **Examples of accepted data**
| name   | logFC | logCPM |    F | PValue |  FDR |
| :----- | ----: | -----: | ---: | -----: | ---: |
| 2L52.1 | -0.26 |   1.52 | 0.25 |   0.63 |    1 |
| aagr-1 | -0.36 |   5.96 | 1.55 |   0.25 |    1 |
| aagr-2 | -0.21 |   7.02 | 0.88 |   0.37 |    1 |
| aagr-3 | -0.21 |   7.22 | 0.80 |   0.40 |    1 |
| aagr-4 |  0.78 |   6.31 | 7.23 |   0.03 |    1 |

(Goh et al., *Aging Cell*, 2018)

<br/>

#### File upload limit

- Single file upload: maximum 10 Mb per upload
- Batch file upload: maximum 50 Mb per upload
- File storage: maximum 100 Mb per session

