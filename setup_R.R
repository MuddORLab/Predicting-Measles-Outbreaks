repos <- "https://cloud.r-project.org"

pkgs <- c(
  # core
  "tidyverse", "dplyr", "tidyr", "readr", "stringr", "ggplot2", "janitor", "here",
  # for trees
  "rsample", "rpart", "rpart.plot", "ipred", "caret", "randomForest",
  "xgboost", "smotefamily", "rPref",
  # for glm
  "glmnet", "glmmTMB", "buildmer", "DHARMa", "pROC",
  "performance", "broom.mixed", "mpath",
  # for clustering
  "cluster", "factoextra", "igraph", "geometry",
  # for SHAP/visualization
  "shapviz", "treeshap", "plotly", "viridis", "stringdist", "tidycensus",
  # Jupyter R kernel
  "IRkernel"
)

missing <- setdiff(pkgs, rownames(installed.packages()))
if (length(missing)) install.packages(missing, repos = repos)

IRkernel::installspec()
cat("The environment for running all notebooks that contains R code has been set up successfully. You may run the notebooks now.\n")
