# Measles Outbreak and Public Policy Reluctance - Research Summary

Vaccination reluctancy has been increasing in recent years and with this increase, there have been outbreaks of illnesses such as measles. The purpose of this research is to define a relationship between measles outbreaks and conscientious exemption rates in places where measles outbreaks are more prevalent in 2025.

We are looking at measles outbreaks at the county level from Texas. We also have data from Canada as a way to compare and contrast the magnitude of outbreaks between the United States and other countries.

# Research Components

Research has included:
- Data collection from Canada and Texas
- Sentiment analysis of social media posts
- Statistical analysis

A large component of the research is obtaining data and cleaning it as a way to make the data easier to interpret, thus a majority of the files in the repository are notebooks that clean the data and the resuling CSVs. A majority of the data collected is from the Census, American Community Survey, and Texas Department of State Health Services. The data we are collecting contains information regarding vaccine sentiments, vaccine rates, social media posts, and socioeconomic factors such as housing. We gather this type of data from Texas primarily to create predictive statistical models to determine what is correlated to measle outbreaks.

For the statistical analysis, we have created Classification and Regression models to mimick human decision making, using the data collected as factors in the decision making. With the results, we analyze what behavior and socioeconomic factors are significant to these outbreaks.

Social media posts from Bluesk and Twitter (X) are used in the sentiment analysis, meaning we are useing machine learning models to analyze vaccine sentiment prescense in social media posts.

# Goal

With the results of the sentiment and statistical analysis, we are hoping that this can inform public policy in a way that can decrease these measles outbreaks.

# Quick Start
Visual Studio Code was primarily used and the following instructions are based on the use of VS Code

### Prerequisites
 - Python extension by Microsoft
 - Jupyter extension by Microsoft
 - R extension for VS Code by REditorSupport

### Installation

1. **Clone Git Repository**
   ```bash
   git clone <repo url>
   ```
2. **Installing libraries**

   Here's a list of R packages one must have to ensure the jupyter notebooks run:

   - readr
   - rsample
   - rpart
   - dplyr
   - rpart.plot
   - ipred
   - caret
   - smotefamily
   - janitor
   - tidyverse
   - xgboost
   - rPref
   - ggplot2
   - igraph
   - plotly
   - randomForest
   - stringr
   - cluster
   - factoextra

   ```bash
   R
   install.packages("package name")

# Data Sets

We have created datasets with the following data: 
- Behavioral Risk Factor Surveilliance System (BRFSS)
- CDC Social Vulnerability Index (SVI)
- Texas Census 2025
- American Community Survey: Conscientious Vaccine Exemption Rate (CVE), school enrollment, population, Public Health Region (PHR) division, demographic, and socioeconomic factors; we consider this basedata

Here are the dataset names and the data contained in them:
- merged_data: BRFSS, Census, basedata
- merged_with_svi: BRFSS, Census, basedata SVI

# Notebook Descriptions
The following are descriptions of the notebooks in the folder titled 2026SU, which contains the most recent work

### CART_on_raw_data.ipynb
Classification Model, specifically XGBoost model, that uses data that contain NA's

Datasets: merged_data and merged_with_svi

### classification_tree.ipynb
Contains three classification models: Decision Tree, RandomForest, and XGBoost. The data sets don't contain NAs.

Datasets: merged_data and merged_with_svi

### glm.ipynb

Predicitve General Linear Model (GLM)

Datasets: merged_data and merged_with_svi

### logistic_regression.ipynb

Logistic regression model - linear model

Datasets: merged_data

### regression_tree.ipynb

Regression tree - Basic Regression tree model, Regression Random forest model, Regression XGBoost model

Uses both IRkernel and python kernel

Datasets: merged_data and merged_with_svi

# Tech Stack
- R
- Python
