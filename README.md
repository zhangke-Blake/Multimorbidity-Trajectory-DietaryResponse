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

## Environment & Dependencies

### R Packages
The following core packages are required (full list available in the `/functions` folder):
*   **Statistics:** `lmerTest (v3.1.3)`, `ppcor (v1.1)`, `vegan (v2.6.8)`, `bnlearn (v5.1)`, `mediation (v4.5.0)`
*   **Machine Learning:** `randomForest (v4.7.1.2)`, `iml`
*   **Bioinformatics:** `clusterProfiler (v4.14.4)`, `org.Hs.eg.db (v3.20.0)`
*   **Visualization:** `ggplot2 (v3.5.1)`, `ggpubr (v0.6.0)`, `igraph (v2.1.2)`
*   **Data Handling:** `Hmisc (v5.2.2)`, `compositions (v2.0.8)`

## Usage
1.  **Set Directories:** Define your `root` and `workpath` in the R environment.
2.  **Sequential Execution:** Run scripts from `Part 0` to `Part 5` sequentially to maintain data dependencies.
3.  **Outputs:** Statistical results and processed data are automatically saved in subfolders (e.g., `./part1_aging/`, `./part5_proteomics/`).

## Contact
[Ke Zhang / Lab of Precision nutrition and computational medicine]  
[zhangke@westlake.edu.cn]
