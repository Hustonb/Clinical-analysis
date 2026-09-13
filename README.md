# Associations Between Demographic and Workplace Characteristics and Treatment Seeking Behavior for Mental Health Issues in Tech Workplaces.

## Link to Final Analysis/Report

ADD LINK HERE

## Overview

This project seeks to investigate the associations between different demographic and workplace predictors on whether or not individuals seek out treatment for mental health issues in the technology industry.

The analysis uses data from the 2014 Mental Health in Tech Survey
conducted by Open Sourcing Mental Illness (OSMI).

## Research Question

Which demographic and workplace characteristics are associated with seeking treatment for mental health conditions among technology professionals?

## Data

The dataset contains responses from technology professionals who
participated in OSMI's 2014 Mental Health in Tech Survey.

Source: [Kaggle – Mental Health in Tech Survey](https://www.kaggle.com/datasets/osmi/mental-health-in-tech-survey)

## Methods
-Import data
-Data preprocessing/Address missingness
-Data Exploration and visualization
-Statistical analysis
-Verify logistic regression model assumptions
-Asses multicollinearity with VIF and GVIF
-Fit univariable and multivariable logistic regression models
-Hosmer-Lemeshow goodness of fit test

## Key Findings

- Male participants had lower odds of seeking treatment than female
  participants.
- Participants with a family history of mental illness had higher
  odds of seeking treatment.
- Workplace characteristics showed comparatively limited evidence
 of independent associations after adjustment.

## Repository Structure

- `R/01_import.R` – Data import
- `R/02_cleaning.R` – Data cleaning and preprocessing
- `R/03_descriptive.R` – Descriptive analysis and visualization
- `R/04_modeling.R` – Logistic regression modeling
- `R/05_diagnostics.R` – Model diagnostics
- `TreatmentAnalysis.qmd` – Reproducible report

## Full Analysis

**[View the full analysis report](YOUR-GITHUB-PAGES-LINK)**

## Tools

- R
- RStudio
- Quarto
- ggplot2
- dplyr
- gtsummary
- gt
- car

## Author

Benedict Huston
