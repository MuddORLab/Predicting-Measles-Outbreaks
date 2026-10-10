# Clear working environment
rm(list=ls())

# Load libraries
source("1.LoadLibraries.R")


# Load raw data and create root data files
source("2.LoadRawData.R")
# Reads in:
# BRFSS 2014 (already scraped and with matching names)
# BRFSS 2024
# SVI
# Outbreaks
# Census
# Enrollment and CVE
# Outputs BRFSS+SVI+census+other PHR/County-level data for prediction models and 
# SVI+census+other county-level data for clustering
#
# Data cleaning (e.g. drop yes/no redundant features) should occur here too.

###################### CLUSTERING ############################


# Clustering
source("3.Clustering.R")
# Loads in SVI+census+other county-level data for clustering
# Saves assigned clusters and cluster centers as presented in paper
# Saves graphs


###################### CLASSIFICATION ############################
# NO LONGER NEED FILE 4 BECAUSE IT IS HANDLED IN FILE 2.
# Create training and testing partitioning for classification methods
#source("4.ClassificationSplitData.R")
# Reads in BRFSS+SVI+census+other PHR-level data for models
# Saves training and testing files for use by all classification methods
# Saves folds for use by all classification methods


# Classification tree
source("5.ClassificationTreeAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for classification tree
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.

# Classification Random Forest 
source("6.ClassificationRandomForestAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for random forest
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis, including SHAP
# Saves any results reported in the manuscript.

# Classification XGBoost
source("7.ClassificationXGBoostAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for XGBoost
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis, including SHAP
# Saves any results reported in the manuscript.

# Logistic Regression
source("8.LogisticRegression.R")
# Reads in saved training and testing files and folds
# Parameter tuning for logistic regression
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.



###################### REGRESSION ############################

#NO LONGER NEED FILE 9 BECAUSE IT IS HANDLED IN FILE 2.

# Create training and testing partitioning for regression methods
#source("9.RegressionSplitData.R")
# Reads in BRFSS+SVI+census+other PHR-level data for models
# Saves training and testing files for use by all regression methods
# Saves folds for use by all regression methods


# Regression tree
source("10.RegressionTreeAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for regression tree
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.

# Regression Random Forest 
source("11.RegressionRandomForestAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for random forest
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.

# Regression XGBoost
source("12.RegressionXGBoostAnalysis.R")
# Reads in saved training and testing files and folds
# Parameter tuning for xGBoost
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.

# Negative Binomial Regression --> Gaussian Regression on log counts
source("13.LogGaussianRegression.R")
# Reads in saved training and testing files and folds
# Parameter tuning for Gaussian GLM on log(counts)
# Saves any intermediate results used to choose optimal parameter values as included in manuscript
# Fits and saves tuned model
# Conducts analysis
# Saves any results reported in the manuscript.

