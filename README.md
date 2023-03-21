# Assessing US state-level COVID-19 outcomes and their economic and educational trade-offs
This repository contains code that performs the regression analysis to identify factors associated with SARS-CoV-2 infection rate and COVID-19 death rate, as well as economic (GDP, employment) and educational (4th grade math and reading test scores) trade-offs. This analysis has been accepted for publication in The Lancet under the title "Assessing COVID-19 pandemic policies and behaviours and their economic and educational trade-offs across US states from January, 2020, to July, 2022: an observational analysis."

## Purpose
The purpose of this repository is to make the analytic code for this research study available as part of [Guidelines for Accurate and Transparent Health Estimates Reporting (GATHER)](http://gather-statement.org/) compliance.

## Usage
Inputs required for the code to run are:
1. Valid paths to directories must be specified as input_root, covid_dir, and out_root in flexible_model_build_with_draws.R
2. A dependent variable for the regression analysis must be specified on line 18. Valid options are "infections", "deaths", "employment", "gdp", and "edu". If "edu", a valid subject ("math" or "reading") and grade (4 or 8) must also be selected.
3. A model specification file must be saved to the input directory following the example document provided in this repository. 
4. A data file containing all regression variables.

## Authors
This code was written by Emma Castro, Amy Latsuka, Kayleigh Bhangdia, and Jeremy Dalos, with guidance from corresponding author Joe Dieleman.
