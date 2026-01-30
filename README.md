# Longitudinal multimorbidity trajectories shape personalized glycemic patterns

## Overview
This repository contains the R source code for a comprehensive bioinformatics study investigating the associations between aging-related multimorbidity trajectories and personalized glycemic patterns. The analysis utilizes Continuous Glucose Monitoring (CGM) derived metrics, clinical phenotypes, personalized glycemic sensitivity index (PGS), and serum proteomics.

## Analysis pipeline

### Part 0: Aging-related multimorbidity trajectories linked to divergent glycemic patterns
*   Correlation analysis: partial Spearman correlations between uniqueness of multimorbidity and uniqueness indices of various CGM traits.
*   Slicing window analysis: calculation of the coefficient of variation (CV) of glycemic traits across age groups to evaluate glycemic stability during aging.

### Part 1: Disease duration of morbidities and CGM-derived daily glycemic traits
*   Linear modeling: associations of disease durations with daily glycemic traits (e.g., eA1C, MAGE, CV).
*   Disease network: implementation of Bayesian Network (BN) structure learning and causal inference to map the complex interdependencies among various diseases.
*   Interaction analysis: investigation of the difference in longitudinal trajectories of multimorbidity stratified by baseline dyslipidemia status.

### Part 2: Systemic multimorbidity index (MMI-system) and daily glycemic traits
*   Stratified comparison: comparison of glycemic variability across different levels of multimorbidity severity by using multi-variable linear regression analysis.

### Part 3: Systemic multimorbidity index, PGS, and dietary responses
*   Meal response analysis: Wilcoxon tests and linear regression to observe how PGS associates with postprandial glycemic responses to standardized meal tests.
*   Stratified comparison: comparison of the relationship between MMI-system and PGS stratified by multimorbidity severity.

### Part 4: Prediction modeling
*   Machine learning: random forest models to predict CGM-measured daily traits and postprandial responses.
*   Performance evaluation: comparison of basic models (standard clinical markers) versus integrated models (incorporating the multimorbidity trajectories and related traits).

### Part 5: Systemic multimorbidity index and serum proteomics
*   Identification of multimorbidity-related protein signatures: applying mixed-linear models to identify multimorbidity-related proteins using longitudinal proteomic data.
*   Network topology: analyzing protein-protein correlation networks across different health states using topological features.
*   Functional enrichment: GO (Gene Ontology) enrichment analysis for biological pathways associated with metabolic comorbidity.
*   Mediation analysis: testing the mediation effect of specific proteins between systemic multimorbidity severity and PGS.
*   PGS prediction and SHAP integration: using SHAP (SHapley Additive exPlanations) values to interpret the contribution of individual proteins to the prediction of PGS.

## Data visualization
Plots.R serves as the core visualization module, containing scripts for generating all primary figure panels using processed outputs derived from the analysis pipeline.

## R packages
The full list of required packages and functions is available in the 'function' folder. Core dependencies include:
Hmisc v.5.2.2, randomForest v.4.7.1.2, lmerTest v.3.1.3, igraph v.2.1.2, clusterProfiler v.4.14.4, org.Hs.eg.db v.3.20.0, mediation v.4.5.0, ppcor v.1.1, ggplot2 v.3.5.1, ggpubr v.0.6.0, compositions v.2.0.8, vegan v.2.6.8, bnlearn v.5.1.

## Usage
Define your root and workpath directories in the R environment. Run the scripts sequentially from Part 0 to Part 5 to maintain data dependencies. Results will be saved in subfolders corresponding to each part (e.g., /part1_aging, /part5_proteomics).
