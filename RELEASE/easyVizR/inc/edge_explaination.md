---
title: "Edge parameters: Determine degree of gene overlap"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---

An edge represents the degree of gene overlap that exists between two gene sets. Adjusting edge parameters controls the number of edges to be created in the network.

<br/>

#### **Jaccard Coefficient**
`[size of (A intersect B)] / [size of (A union B)]`

* The default
* Recommended when relations are not expected to occur between large-size and small-size gene sets.


<br/>

#### **Overlap Coefficient**
`[size of (A intersect B)] / [size of (minimum( A , B))]`

* Recommended when relations are expected to occur between large-size and small-size gene sets, as in the case of the Gene Ontology (GO).
* When the network has several large gene sets excessively connected to many other gene-sets, we recommend switch to the Jaccard Coefficient.


<br/>


#### **Combined Coefficient**
`(k * Overlap) + ((1-k) * Jaccard)`
, where `k = Combined Constant`

* A merged version of the Overlap and Jaccard coefficients.
* When k = 0.5, the Combined Coefficient is the average between the Overlap and Jaccard.


<br/>
To learn more, see [Using networks to measure similarity between genes: association index selection](https://pubmed.ncbi.nlm.nih.gov/24296474/), [Tips on Parameter Choice](https://enrichmentmap.readthedocs.io/en/latest/Parameters.html).
