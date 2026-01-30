# Longitudinal multimorbidity trajectories shape personalized glycemic patterns
This repository contains the R source code for a comprehensive bioinformatics study investigating the associations between aging-related multimorbidity trajectories and personalized glycemic patterns. The analysis utilizes Continuous Glucose Monitoring (CGM) derivied metrics, clinical phenotypes, personalized glycemic sensitivity index (PGS), and serum proteomics. The description of main analysis pipeline is as shown below.



## Part 0: Aging-related multimorbidity trajectories linked to divergent glycemic patterns. 
 __1) Correlation analysis:__ Partial Spearman correlations between uniqueness of multimorbidity and uniqueness indices of various CGM traits (daily means, nighttime/daytime metrics, and meal-based responses). Euclidean distance-based "uniqueness" scores to quantify how individual metabolic profiles deviate from the population average and how this relates to aging and disease trajectories.

 __2) Slicing window analysis:__ Calculates the Coefficient of Variation (CV) of glycemic traits across age groups using a sliding window approach to evaluate glycemic stability during aging. 


## Part 1: Disease duration of morbidities and CGM-derivied daily glycemic traits.
__1) Linear modeling:__ Associations of disease durations with daily glycemic traits (e.g., eA1C, MAGE, CV).

__2) Disease network:__ This script implements Bayesian Network (BN) structure learning and causal inference to map the complex interdependencies among various diseases. It utilizes bootstrapping and tabu search to construct a consensus Directed Acyclic Graph (DAG), subsequently quantifying causal relationships through probability estimation.

__3) Interaction analysis:__ Investigating the difference in longitudinal trajectories of multimorbidity stratified by baseline dyslipidemia status.


## Part 2: Systemic multimorbidity index (MMI-system) and daily glycemic traits.
__1) Stratified comparison:__ Comparing glycemic variability across different levels of multimorbidity severity (Healthy, Single Disease, Mild Comorbidity, Severe Comorbidity) by using multi-variable linear regression analysis. 


## Part 3: Systemic multimorbidity index and PGS, and Dietary responses.
__1) Meal response analysis:__ Wilcoxon tests and linear regression to observe how PGS associated with postprandial glycemic responses to standardized meal tests (Refined & Whole grain).

__2) Stratified comparison:__ Comparing the difference in the relationship between MMI-system and PGS stratified by multimorbidity severity.


## Part 4: Prediction modeling
__1) Machine learning:__ Random Forest models to predict CGM-measured daily traits and postprandial responses.

__2) Performance evaluation:__ Comparison of "Basic" models (standard clinical markers) versus "Integrated" models (incorporating the Multimorbidity trajectories and related traits). 


## Part 5: Systemic multimorbidity index and serum proteomics
__1) Identification of multimorbidity-related protein signatures:__ Applying mixed-linear models to identify Multimorbidity-related proteins using longitudinal proteomic data.

__2) Network topology:__ Analyzing protein-protein correlation networks across different health states (Healthy -> Severe) using topological features (e.g. density, transitivity, centrality).

__3) Functional enrichment:__ GO (Gene Ontology) enrichment analysis for biological pathways associated with metabolic comorbidity.

__4) Mediation Analysis:__ Testing the mediation effect of specific proteins and the weighted protein score between systemic multimorbidity severity and PGS.

__5) PGS prediction and SHAP integration:__ Using SHAP (SHapley Additive exPlanations) values to interpret the contribution of individual proteins to the prediction of PGS.


## Data visualization
Plots.R serves as the core visualization module, containing scripts for generating all primary figure panels. It consumes the processed outputs derived from the analysis pipeline (Part 0 to Part 5) to produce publication-quality visualizations.


## R Packages
The following R packages are required to run the core analysis, and the full list of required packages and functions are available in the 'function' folder:

Hmisc v.5.2.2, randomForest v.4.7.1.2, lmerTest v.3.1.3, igraph v.2.1.2, clusterProfiler v.4.14.4, org.Hs.eg.db v.3.20.0, mediation v.4.5.0, ppcor v.1.1, ggplot2 v.3.5.1, ggpubr v0.6.0, compositions v.2.0.8, vegan v.2.6.8, bnlearn v.5.1. 


## Usage
Define your root and workpath directories in the R environment.
Run the scripts sequentially from Part 0 to Part 5 to maintain data dependencies.
Results (statistics and processed data) will be saved in subfolders corresponding to each part (e.g., /part1_aging, /part5_proteomics).




