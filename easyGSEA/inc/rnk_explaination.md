---
title: "Ranked list file format (*.rnk)"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---

#### **RNK file**

The RNK file contains a single, rank ordered gene list (not gene set) in a simple newline-delimited text format. It has two columns: the first column contains **genes** while the second contains **rank scores**. It is used when you have a pre-ordered ranked list that you want to analyze with GSEA. The list need not be sorted.


We support both comma- and tab-delimited text files.

<br/>


You can try our example RNK file (Goh et al., *Aging Cell*, 2018; Blanco-Melo D et al., *Cell*, 2020) by clicking the `loadExampleRNK` button below. Please make sure you have selected your species of interest. The file will be automatically loaded.

<button id="loadExampleRNK" type="button" class="btn action-button btn-warning">loadExampleRNK</button>

<div id="example1" class="shiny-html-output"></div>


<br/>

#### **Convert differential gene expression (DEG) analysis file into RNK**

It is also possible to generate an RNK by converting from differential expression analysis results by tools such as DESeq2, edgeR, and limma. The results should be saved in a comma- (.csv) or tab-delimited (.txt/.tab) text file. Our app will automatically detect it. You will need to specify three columns: **genes**, **logFC** and **p-value**. Our app will generate the RNK for you. You can try our sample DEG file (Goh et al., *Aging Cell*, 2018; Blanco-Melo D et al., *Cell*, 2020) by clicking the `loadExampleDEG` button below.

<button id="loadExampleDEG" type="button" class="btn action-button btn-warning">loadExampleDEG</button>

<div id="example2" class="shiny-html-output"></div>

<br/><br/>
To learn more, see <a href="https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#RNK:_Ranked_list_file_format_.28.2A.rnk.29" target="_blank">Data formats - Ranked Gene Lists</a>
