---
title: "pre-ranked GSEA p & padj selection"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---


Different sets of thresholds for P value (P) and adjusted P value (P.adj) are suggested for pre-ranked GSEA analysis:  

##### **Very permissive:**

* P < 0.05
* P.adj < 0.25

##### **Moderately permissive:**
* P < 0.01
* P.adj < 0.1

##### **Moderately conservative:**
* P < 0.005
* P.adj < 0.075

##### **Conservative:**
* P < 0.001
* P.adj < 0.05

<br/>
To learn more, see <a href="https://readthedocs.org/projects/enrichmentmap/downloads/pdf/latest/" target="_blank">Tips on Parameter Choice - P-value and FDR thresholds</a>

<br/>
Reference:  
Merico, D., Isserlin, R., Stueker, O., Emili, A. and Bader, G.D., 2010. Enrichment map: a network-based method for gene-set enrichment visualization and interpretation. *PloS one*, 5(11), p.e13984.
