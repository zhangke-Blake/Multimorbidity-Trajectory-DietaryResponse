# Longitudinal Multimorbidity Trajectories Shape Personalized Glycemic Patterns

## Overview
This repository contains the R source code for a comprehensive bioinformatics study investigating the associations between aging-related multimorbidity trajectories and personalized glycemic patterns. The analysis integrates high-resolution Continuous Glucose Monitoring (CGM) metrics, clinical phenotypes, Personalized Glycemic Sensitivity index (PGS), and serum proteomics from the GNHS cohort.

## Analysis Pipeline

### Part 0: Aging-related Multimorbidity and Glycemic Divergence
*   **Correlation Analysis:** Partial Spearman correlations between the "uniqueness" of multimorbidity and glycemic traits. We utilize Euclidean distance-based uniqueness scores to quantify individual deviations from population averages.
*   **Slicing Window Analysis:** Assessment of glycemic stability (Coefficient of Variation, CV) across age groups using a sliding window approach.

### Part 1: Disease Duration and Daily Glycemic Traits
*   **Linear Modeling:** Evaluating how disease duration impacts daily glycemic metrics (e.g., eA1C, MAGE, CV).
*   **Disease Network:** Implementation of Bayesian Network (BN) structure learning and causal inference (bootstrapping + tabu search) to map interdependencies among diseases via Directed Acyclic Graphs (DAG).
*   **Interaction Analysis:** Longitudinal trajectory analysis of multimorbidity stratified by baseline dyslipidemia status.

### Part 2: Systemic Multimorbidity Index (MMI-system)
*   **Severity Stratification:** Multi-variable linear regression comparing glycemic variability across health states: Healthy, Single Disease, Mild Comorbidity, and Severe Comorbidity.

### Part 3: MMI, PGS, and Dietary Responses
*   **Meal Response Analysis:** Wilcoxon tests and linear regression investigating genetic risk (PGS) associations with Postprandial Glycemic Responses (PPGR) to standardized refined and whole-grain meals.
*   **PGS-MMI Interaction:** Analysis of the relationship between systemic multimorbidity and genetic predisposition.

### Part 4: Predictive Modeling
*   **Machine Learning:** Random Forest (RF) models for predicting daily and postprandial glycemic traits.
*   **Performance Evaluation:** Comparative analysis of "Basic" models (clinical markers) vs. "Integrated" models (incorporating multimorbidity trajectories).

### Part 5: Multimorbidity-Related Serum Proteomics
*   **Signature Identification:** Mixed-linear models used to identify multimorbidity-related protein signatures from longitudinal data.
*   **Network Topology:** Analysis of protein-protein correlation networks (density, transitivity, centrality) across health states.
*   **Functional Enrichment:** GO (Gene Ontology) enrichment for biological pathways associated with metabolic comorbidity.
*   **Mediation Analysis:** Testing proteins/weighted scores as mediators between multimorbidity severity and PGS.
*   **Explainable AI:** Integration of SHAP (SHapley Additive exPlanations) values to interpret protein contributions to PGS prediction.

## Data Visualization
The `Plots.R` script serves as the core visualization module. It processes the statistical outputs from Parts 0-5 to generate publication-quality figures.

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
[Your Name/Lab Name]  
[Your Email Address]
