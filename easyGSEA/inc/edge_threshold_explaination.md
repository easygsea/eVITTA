---
title: "Edge parameter thresholds"
# author: "Jean"
# date: "21/06/2020"
output: html_document
runtime: shiny
---

#### Use a *heigher* (more conservative) threshold if you find the enriched gene sets are too connected with each other. Likewise, switch to a *lower* (more permissive) threshold if you hardly find any connections between enriched terms.


<br/>

#### **Jaccard Coefficient**

* 0.5 is very conservative.
* 0.25 is the default. Moderately conservative.

<br/>

#### **Overlap Coefficient**

* 0.5 is moderately conservative. Recommended for most of analyses.
* 0.3 is permissive. Might result in a messier network.

<br/>


#### **Combined Coefficient**

* k = 0.5, the average between the Overlap and Jaccard.
* Larger k gives higher rate on Overlap.
* Smaller k gives higher weight on Jaccard.

