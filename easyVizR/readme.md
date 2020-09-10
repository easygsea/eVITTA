# easyVizR User Guide

easyVizR (read: *easy-vise-R*) provides various visualization modules designed for comparing and making inferences from multiple sets of expression data. 

## Supported data types

Any dataset with a main fold-change statistic (e.g. logFC), P value, and FDR value are accepted.

This includes but is not limited to the following:

- Differential expression (DE) data from RNA-seq / microarray
- Gene Set Enrichment Analysis (GSEA) from RNA-seq / microarray
- Over-Representation Analysis (ORA) from differentially expressed gene lists

Note that it is *strongly* recommended to use **the whole dataset without any cutoffs or filters**. You can apply filters in the visualization interface, which will be reflected dynamically in the visualizations. Take advantage of this reactivity to interrogate the data in greater depth.

## STEP 1. UPLOAD FILES

#### Example dataframe structure

The input dataframe should have at least four columns: **Name**, **Stat** (the main statistic, e.g. logFC), **PValue**, **FDR**. A minimal example looks like this:

| Name   | logFC | PValue |  FDR |
| :----- | ----: | -----: | ---: |
| 2L52.1 | -0.26 |   0.63 |    1 |
| aagr-1 | -0.36 |   0.25 |    1 |
| aagr-2 | -0.21 |   0.37 |    1 |
| aagr-3 | -0.21 |   0.40 |    1 |
| aagr-4 |  0.78 |   0.03 |    1 |

Currently, **both** PValue and FDR columns are required. If your data only has one of these columns, you can bypass this requirement by manually duplicating the available column, and avoiding to use the duplicated column in visualizations (*NOT RECOMMENDED*).

#### Single File Upload

You can upload a single .txt or .csv file (comma separated).

After uploading, you will be asked to specify the following things:

- Name the dataset; (Default: the file name)
- Name the main statistic;
- Select the correct columns for each required field;
- Load additional columns. Some of these may be used in certain visualizations. (e.g. you can plot logCPM in <u>heatmap</u> or <u>bar plot</u>.)

#### Batch upload

Select a folder containing multiple .txt or .csv files (comma separated).

Currently, **all the files must have the same column names**. Files having different column names will be omitted. If that happens, you can either change the column names, or upload them one by one through <u>single file upload</u>.

#### From existing dataset

You can:

- Duplicate a currently loaded dataset (set the P filter and FDR filter to 1, |Stat| filter to 0, and select All for sign)
- Add a filtered version of a loaded dataset. Note that this removes genes that do not fulfill the filters. (Not recommended to use this feature, as it will introduce empty cells later on. Please use the filters in the Multiple tab instead.)

#### Manage datasets

Large dataframes can eat up lots of resources on the server side and lengthen the processing time. You can remove unneeded datasets here and free up some memory.

## STEP 2. SELECT DATA TO VISUALIZE

In the Visualizations interface, you will be asked to select two or more datasets to visualize.

To proceed, the datasets selected must have at least 1 shared row (i.e. at least 1 matched entry in the "Name" column).

Click "Visualize" when ready.

## STEP 3. SET UP FILTERS

A common aim in omics is to compare gene expression changes in multiple conditions or backgrounds. Filtering strategies help us to differentiate what counts as an interesting "change", and what is not. 

In the initial stages of data exploration, one should try different filters to see which ones capture the most biologically interesting changes without sacrificing specificity.

All options for filtering are on the top right corner. There are two main ways to filter:

- Manually enter a list of names
- Specify P, FDR, Stat filters for each dataset or all datasets

#### Manually filter by a list of names

If you have specific genes in mind, open the dropdown and enter them here (separated by new line).

Currently, the case doesn't matter, but the string must match exactly.

#### Specify filters

Here you can filter each dataset by:

- PValue <= value (default 0.05)
- FDR <= value
- Stat can be filtered by value and sign. e.g. "|Stat|>1.5, +" gives you Stat > 1.5 in the positive direction, and "|Stat|>1.5, All" gives you both directions.

You can change all the individual filters simultaneously by applying a global filter in the leftmost box. Note that this will overwrite existing filters.

#### View filtered gene lists

This dropdown shows the filtered gene lists (or pathway lists, etc.) depending on your current filters.

You can copy and paste this into other tools like EnrichR if needed.

## STEP 4. ANALYZE AND SELECT INTERSECTION

Venn and UpSet plots below will show you how well the gene lists overlap with each other, depending on current filters. (only visible for n<=5)

#### Select intersection

Here you can select an intersection from the Venn diagram that you are interested in. 

The selection consists of TRUE, FALSE and Ignore options for each dataset. 

| Option | Contained in the filtered gene list? | Satisfy the filters? | Relation to Venn circle     |
| ------ | ------------------------------------ | -------------------- | --------------------------- |
| TRUE   | YES                                  | YES                  | included in the circle      |
| FALSE  | NO                                   | NO                   | excluded from the circle    |
| Ignore | may or may not                       | may or may not       | may be included or excluded |

Using the Venn as a guide, combine the selections to get the intersection you want. 

Verify that you have selected the correct intersection by comparing the number of rows in the table to the Venn counts, and by checking the "Active filters" description.

## STEP 5. VISUALIZE INTERSECTION

The visualizations will be drawn based on the currently selected intersection, which is shown in the bottom table. You can change this any time, and the visualizations will be updated accordingly.

#### Heatmap

By default, the main Stat (i.e. logFC/ ES) is plotted. You can also plot a numerical statistic that is available for all the selected datasets, e.g. logCPM.

Hover label will show all the shared parameters, numeric or not. For instance, if all datasets possess a column for GSEA leadingEdge, it will be displayed in the hover label.

#### 2D Scatter

Select two datasets in the Scatter panel to plot a 2D scatter. 

##### Correlation

For the 2D scatter, the correlation line and the correlation report are also shown on the left.

#### 3D Scatter

Select three datasets in the Scatter panel to plot a 3D scatter.

#### Single dataset visualizations

##### Volcano

Volcano plot shows the -log10(PValue) against the main Stat (e.g. logFC). It usually shows 2 colors based on cutoff.

By default, only the selected intersection is included in the graph. If you want to see how the distribution for this intersection compares with all the genes, select "Context" mode; genes not included in this intersection will be shown on the background.

##### Bar plot

Bar plot is only available if the number of genes in selected intersection <= 15.



# FAQ

#### Uploading files

Q: I'm seeing a lot of numbers in the header selection, why is that?

- A: Check if your files have a header column. By default, the app uses the first row as the header column.

#### Filtering and Visualization

Q: I'm using this to plot GSEA data. I included the leadingEdge at upload, but now it won't show up in the heatmap, help?

- A: Check if all your selected datasets have the leadingEdge column, and if the column has the same name. The heatmap only shows the same name. 

Q: I chose to filter by gene list, but now nothing shows up in the visualizations and the table and I get no "gene not found" message.

- A: Most likely your genes don't fulfill the active filters and are excluded. Try removing all the filters.



## Known issues

- **Safari users**: for the plotly interactive graphs, the top right "download as png" button might not work. This is a bug within plotly itself. Please download as html or take a screenshot instead.