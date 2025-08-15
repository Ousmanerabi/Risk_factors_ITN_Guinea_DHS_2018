# Factors Associated with the Use of Insecticide-Treated Nets in Guinea  
*Analysis of the 2018 Demographic and Health Survey*

---

## Overview
This repository contains the R code and analysis pipeline for the article:  
**"Factors associated with the use of insecticide-treated nets in Guinea: an analysis of the 2018 Demographic and Health Survey"**.  
The objective was to identify household and individual-level determinants of ITN ownership and use, in order to inform national malaria prevention strategies.

---

## Data
- **Source:** Guinea DHS 2018 (Demographic and Health Survey)  
- **Access:** Available from the [DHS Program](https://dhsprogram.com/data/available-datasets.cfm) upon request  
- **Key variables:** Household wealth quintiles, education, residence type, household composition, ITN ownership, ITN use

---

## Methods
- Complex survey analysis accounting for weights, clusters, and strata
- Logistic regression for ITN ownership and ITN use among those with access
- Bivariate, univariate, and multivariate models
- Analysis performed in **R**

---

## Repository Structure
Indicators/ # Scripts for calculating household and individual-level indicators
Household_and_individual_level_indicators.R
Recoding of variables in the PR file.R

Risk_factors_Analysis/ # Scripts for bivariate, univariate, and multivariate analysis
Househod_ITN_ownership_risk_factors.R
ITN_use_among_with_access_risk_factors.R

yaml
Copier
Modifier

---

## How to Use
1. Run `Household_and_individual_level_indicators.R` in the `Indicators/` directory to recode the selected variables at the household level and calculate the indicators.
2. Execute `Recoding of variables in the PR file.R` script.
3. Run `Househod_ITN_ownership_risk_factors.R` in the `Risk_factors_Analysis/` folder to estimate risk factors for ITN ownership.
4. Run `ITN_use_among_with_access_risk_factors.R` in the `Risk_factors_Analysis/` folder to estimate risk factors for ITN use among those with access.

---

## Impact
- Provided evidence to the National Malaria Control Program for targeted ITN distribution and behavior change communication strategies
- Supports academic research and programmatic decision-making on malaria prevention in Guinea

---

## Contact
For any issues about the code, please contact:  
**Ousmane Diallo** — [ousmanerabi12@gmail.com](mailto:ousmanerabi12@gmail.com)