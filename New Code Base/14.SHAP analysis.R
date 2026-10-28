rm(list = ls())
test <- read_csv("intermed_class_data/testclass.csv")
train <- read_csv("intermed_class_data/trainclass_smote.csv")
folds <- readRDS("intermed_class_data/folds_class.rds")

#Loading models
rforest_model <- readRDS("New Code Base/results/class_rf_model.rds")
boost_model <- readRDS("New Code Base/results/class_xgb_model.rds")

#Importance variables of the model
importance_vars_rf <- c("enrollment", "ep_otherrace", "population",
                       "density", "ep_aian", "ep_hburd", "ep_asian",
                       "pct_uninsured", "overweight_or_obese_yes_pct_diff",
                       "ep_munit")

importance_vars_xgb <- read_csv("New Code Base/results/class_xgb_important_vars.csv")
importance_vars_xgb <- importance_vars_xgb$"Feature"[2:12] #top 10 features in xgb boost

combined_importance_vars <- list(importance_vars_rf, importance_vars_xgb)
common_vars <- Reduce(intersect, combined_importance_vars)

#setting up variables for SHAP analysis
y_test  <- as.numeric(as.character(train$outbreak))

#Random Forest variables
c <- 0.4
cutoff <- c(c, 1-c)
pred_rf <- predict(rforest_model, newdata = test, type = "class", cutoff=c(c, 1-c))

#XGBoost variables
cutoff_xgb <- 0.5
X_train <- as.matrix(train_balanced[, names(train_balanced) != "outbreak"])
y_train <- as.numeric(as.character(train_balanced$outbreak))

X_test  <- as.matrix(test[, names(test) != "outbreak"])
y_test  <- as.numeric(as.character(test$outbreak))

dtrain  <- xgb.DMatrix(data = X_train, label = y_train)
dtest   <- xgb.DMatrix(data = X_test,  label = y_test)

pred_xgb <- as.integer(predict(boost_model, dtest) >= cutoff_xgb)

#Random Forest SHAP plots
feat       <- setdiff(names(test), "outbreak") #getting all features besides outbreak
X_train_df <- as.data.frame(train_balanced[, feat])   # predictors the forest was trained on
X_expl     <- as.data.frame(test[, feat])             # rows to explain (test set)

unified <- randomForest.unify(rforest_model, X_train_df)
ts      <- treeshap(unified, X_expl)
shp_rf  <- shapviz(ts, X = X_expl)

X <- get_feature_values(shp_rf)

#may need to add more predictors depending on beeswarm plot produced without logging predictor shap values
log_vars <- intersect(c("enrollment", "density", "population"),
                      colnames(shp_rf))

for (v in log_vars) {
  X[[v]] <- log1p(X[[v]])   #log1p = log(1 + x); avoids -Inf if any value is 0
}

shp_rf_log <- shapviz(
  get_shap_values(shp_rf),
  X        = X,
  baseline = get_baseline(shp_rf)
)

sv_importance(shp_rf_log, kind = "beeswarm", max_display = 10) +
  ggtitle("Random Forest: Common Variable Directionality") +
  theme(
    axis.text.y  = element_text(size = 13),   # feature name labels on y-axis
    axis.text.x  = element_text(size = 12),
    axis.title.x = element_text(size = 13),   # "SHAP value" axis label
    plot.title   = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 11),   # colorbar tick labels
    legend.title = element_text(size = 12)
  )

#Waterfall plots for incorrect predictions
vect <- as.vector(as.matrix(pred_rf)) #converts entire grid into one long basic vector

#False negatives of Random Forest
fn_ids <- which(y_test == 1 & vect < cutoff)
length(fn_ids)

#fn_id[] will need to be changed
idx <- 1
sv_waterfall(shp_rf_log, row_id = fn_ids[idx], max_display = 20) +
  ggtitle(sprintf("RF Dallam False negative — test row %d  (p = %.3f)", fn_ids[idx], pred_rf[fn_ids[idx]]))

sv_waterfall(shp_rf_log, row_id = fn_ids[2], max_display = 20) +
  ggtitle(sprintf("RF Ector False negative — test row %d  (p = %.3f)", fn_ids[2], pred_rf[fn_ids[2]]))

sv_waterfall(shp_rf_log, row_id = fn_ids[3], max_display = 20) +
  ggtitle(sprintf("Random Forest Lamar False Negative — test row %d  (p = %.3f)", fn_ids[3], pred_rf[fn_ids[3]]))

#False positives of Random Forest
fp_ids <- which(y_test == 0 & vect >= cutoff)
length(fp_ids)
idx <- 1

sv_waterfall(shp_rf_log, row_id = fp_ids[idx], max_display = 20) +
  ggtitle(sprintf("FP Moore — test row %d  (p = %.3f)", fp_ids[idx], pred_rf[fp_ids[idx]]))

#XGB Boost SHAP plots
shp_xgb <- shapviz(boost_model, X_pred = X_test, X = as.data.frame(X_test))
X <- get_feature_values(shp_xgb)
log_vars <- intersect(c("enrollment", "density", "population"), colnames(shp_xgb))
for (v in log_vars) {
  X[[v]] <- log1p(X[[v]])   # log1p = log(1 + x); avoids -Inf if any value is 0
}

shp_xgb_log <- shapviz(
  get_shap_values(shp_xgb),
  X        = X,
  baseline = get_baseline(shp_xgb)
)

#changing feature names
# shp_log_vars <- shp_log[, vars]
# colnames(shp_log_vars) <- c("K-12 enrollment",
#                             "increase in \n overweight/obese \n from 2014",
#                             "% \"Black\" residents",
#                             "estimated % of \n multi unit housing",
#                             "% of residents \n age 65 and older",
#                             "density",
#                             "has had cancer",
#                             "population",
#                             "uninsure %",
#                             "% of housing burden",
#                             "% \"Other Race\" \n residents")

sv_importance(shp_xgb_log, kind = "beeswarm", max_display = 15) +
  ggtitle("Boost Tree: Common Predictors Directionality") +
  theme(
    axis.text.y  = element_text(size = 13),   # feature name labels on y-axis
    axis.text.x  = element_text(size = 12),
    axis.title.x = element_text(size = 13),   # "SHAP value" axis label
    plot.title   = element_text(size = 13, face = "bold"),
    legend.text  = element_text(size = 11),   # colorbar tick labels
    legend.title = element_text(size = 12)
  )

#Waterfall plots - XGB
#CAUTION: ids for false negatives and false positives will be different after rerunning
#False Negatives - XGBoost
fn_ids <- which(y_test == 1 & pred_xgb < cutoff_xgb)
length(fn_ids)
idx <- 1

sv_waterfall(shp_log_vars, row_id = fn_ids[idx], max_display = 20) +
  ggtitle(sprintf("Boost Dallam False Negative — test row %d  (p = %.3f)", fn_ids[idx], pred_xgb[fn_ids[idx]]))

sv_waterfall(shp_log_vars, row_id = fn_ids[2], max_display = 20) +
  ggtitle(sprintf("Boost Lamar False Negative — test row %d  (p = %.3f)", fn_ids[2], pred_xgb[fn_ids[2]]))

#False Positive - XGBoost
fp_ids <- which(y_test == 0 & pred_xgb >= cutoff_xgb)
length(fp_ids)
idx <- 1

sv_waterfall(shp_log_vars, row_id = fp_ids[idx], max_display = 20) +
  ggtitle(sprintf("Boost Howard False Positive — test row %d  (p = %.3f)", fp_ids[idx], pred_xgb[fp_ids[idx]]))

sv_waterfall(shp_log_vars, row_id = fp_ids[2], max_display = 20) +
  ggtitle(sprintf("Boost Moore False Positive — test row %d  (p = %.3f)", fp_ids[2], pred_xgb[fp_ids[2]]))