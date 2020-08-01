---
title: "Ranked list file format (*.rnk)"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---


The RNK file contains a single, rank ordered gene list (not gene set) in a simple newline-delimited text format. It is used when you have a pre-ordered ranked list that you want to analyze with GSEA. The list need not be sorted.

<br/>

#### **Example RNK file**

You can try our example RNK file (Goh et al., *Aging Cell*, 2018) by clicking the `loadExampleRNK` button below. Please make sure you have selected your species of interest. The file will be automatically loaded.

<button id="loadExampleRNK" type="button" class="btn action-button btn-warning">loadExampleRNK</button>

<br/>

#### **Convert RNK from differential expression (DE) analysis file**

It is also possible to generate an RNK by converting from differential expression analysis results by tools such as DESeq2, edgeR, and limma. The results should be saved in a comma- (.csv) or tab-delimited (.txt/.tab) text file. Our app will automatically detect it. You will need to specify three columns: genes, logFC and p-value. Our app will generate the RNK for you. You can try our sample DE file (Goh et al., *Aging Cell*, 2018) by clicking the `loadExampleDE` button below.

<button id="loadExampleDE" type="button" class="btn action-button btn-warning">loadExampleDE</button>

<br/><br/>
To learn more, see [Data formats - Ranked Gene Lists](https://software.broadinstitute.org/cancer/software/gsea/wiki/index.php/Data_formats#RNK:_Ranked_list_file_format_.28.2A.rnk.29).
