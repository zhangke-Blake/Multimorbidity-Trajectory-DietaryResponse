# Longitudinal multimorbidity trajectories shape personalized glycemic patterns
This repository contains the R source code for a comprehensive bioinformatics study investigating the impact of aging on glycemic metabolism, the longitudinal accumulation of disease exposure (LADE), and the proteomic signatures associated with metabolic multimorbidity.

The analysis utilizes Continuous Glucose Monitoring (CGM) data, clinical phenotypes, genomic polygenic scores (PGS), and proteomics from the Guangzhou Nutrition and Health Study (GNHS).

## Project Structure
The analysis is organized into six major parts:

### Part 0: Aging-related multimorbidity trajectories linked to divergent glycemic patterns. 
Correlation Analysis: Spearman correlations between age and various CGM traits (daily means, nighttime/daytime metrics, and meal-based responses).
Slicing Window Analysis: Calculates the Coefficient of Variation (CV) of glycemic traits across age groups using a sliding window approach to evaluate metabolic stability during aging.
Uniqueness Analysis: Euclidean distance-based "uniqueness" scores to quantify how individual metabolic profiles deviate from the population average and how this relates to aging and disease trajectories.

### Part 1: Disease duration of morbidities and CGM-derivied daily glycemic traits.
Linear Modeling: Associating disease onset and duration with daily glycemic traits (e.g., eA1C, MAGE, CV).
Disease Network: Construction of a disease association network using logistic regression to identify co-occurrence patterns of glucose-related diseases.
Interaction Analysis: Investigating the longitudinal trajectories of multimorbidity (e.g., Glucose-Dyslipidemia interactions) using mixed-effect linear models.

### Part 2: Systemic multimorbidity index and daily glycemic traits
Stratified Comparison: Comparing glycemic variability across different levels of multimorbidity (Healthy, Single Disease, Mild Comorbidity, Severe Comorbidity).
Dose-Response: Linear factor modeling to assess the "dose" effect of multiple chronic conditions on glycemic disruption.

### Part 3: Systemic multimorbidity index and PGS, and Dietary responses
Genomic Associations: Evaluating the relationship between Polygenic Risk Scores (PGS) and multimorbidity.
Meal Response Analysis: Wilcoxon tests and linear regression to observe how genetic risk influences postprandial glycemic responses (PPGR) to standardized meal tests (Refined vs. Whole Grain).

### Part 4: Prediction Modeling
Machine Learning: Random Forest models to predict CGM-measured daily traits and postprandial responses.
Performance Evaluation: Comparison of "Base" models (standard clinical markers) versus "Combination" models (incorporating the Multimorbidity-Interaction system).
Metrics: Evaluation via Spearman correlation
.
### Part 5: Systemic multimorbidity index and serum proteomics
CRPs Identification: Mixed-linear models to identify Comorbidity-Related Proteins (CRPs) using longitudinal proteomic data.
Network Topology: Analyzing protein-protein interaction networks across different health states (Healthy -> Severe) using topological features (density, transitivity, centrality).
Functional Enrichment: GO (Gene Ontology) enrichment analysis for biological pathways associated with metabolic comorbidity.
Mediation Analysis: Testing the mediation effect of specific proteins between comorbidity status and genetic risk (PGS).
Explainable AI (SHAP): Using SHAP (SHapley Additive exPlanations) values to interpret the contribution of individual proteins to the prediction of metabolic risk.
Requirements

## R Packages
The following R packages are required to run the scripts:

Data Handling: openxlsx, reshape2, plyr, dplyr
Statistics: lmerTest, mediation, vegan, psych, Hmisc
Machine Learning: caret, randomForest, iml
Network & Visualization: igraph, tidygraph, ggraph
Bioinformatics: clusterProfiler, org.Hs.eg.db, topGO, pathview

## Usage
Define your root and workpath directories in the R environment.
Run the scripts sequentially from Part 0 to Part 5 to maintain data dependencies.
Results (statistics and processed data) will be saved in subfolders corresponding to each part (e.g., /part1_aging, /part5_proteomics).
