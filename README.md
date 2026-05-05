# MasterThesis-SOMA-Quality-Checklist

This repository contains materials, data, documentation, and R code for the master thesis:

**Doing Meta-Meta Right: Developing and Evaluating a Checklist for Assessing the Quality of Second-Order Meta-Analyses**

## Overview

The thesis developed and evaluated the SOMA Quality Checklist, a 25-item checklist for assessing the methodological quality of second-order meta-analyses (SOMAs), also referred to as meta-meta-analyses. The checklist was applied to a corpus of 79 published SOMAs.

This repository provides the materials needed to understand the checklist, reproduce the analyses, and document the literature search and screening process.

## Repository structure

- `code/` contains the R script used for data preparation, descriptive analyses, exploratory factor analyses, latent class analyses, network analysis, and figure generation.
- `data/` contains the input dataset used by the R script and the final data files used for reporting.
- `materials/` contains the final SOMA Quality Checklist, the accompanying codebook, and the practical coding sheet for researchers.
- `search-screening/` contains PRISMA flowcharts and search/screening documentation.
- `results/` can be used to store generated figures and tables.

## Main analysis script

The main analysis script is:

`code/SOMA_Master_coding_Final.R`

The script imports the analysis dataset from:

`data/SOMA-Data_Descriptives_Import.xlsx`

To reproduce the analyses, download or clone the repository, set the working directory to the repository root, and run the script in the `code/` folder. Required R packages are loaded at the beginning of the script.

## Materials

The `materials/` folder contains:

- `SOMA_Checklist_Final.docx`: the final 25-item SOMA Quality Checklist.
- `SOMA_Codebook_Final.docx`: the codebook documenting variable names, item wording, and coding rules.
- `SOMA_Checklist_Coding_Sheet.xlsx`: a spreadsheet version of the checklist intended for researchers who want to apply the checklist to new SOMAs.

## Data

The `data/` folder contains the dataset used for the analyses and final thesis reporting. The main input file used by the R script is:

`SOMA-Data_Descriptives_Import.xlsx`

## Search and screening documentation

The `search-screening/` folder contains documentation of the literature search and study selection process, including PRISMA flowcharts and search/screening documentation from the original and updated searches.

## Notes

The data used in this thesis consist of published second-order meta-analyses and publicly available information from scholarly publications. No personal data or individual-level participant data were collected, stored, or analysed.
