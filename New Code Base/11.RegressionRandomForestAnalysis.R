# SET WORKING DIRECTORY TO THE DIRECTORY WHERE THIS FILE IS STORED.
rm(list=ls())
source("1.LoadLibraries.R")

test <-read_csv("intermed_regress_data/test_clean.csv")
#train_balanced <-read_csv("intermed_regress_data/train_balanced.csv")  # SMOTE IS NOT USED.
train <-read_csv("intermed_regress_data/train_clean.csv")
folds <-readRDS("intermed_regress_data/folds.rds")



# Log transform outbreak
# Need to store column separately because otherwise rpart uses original outbreak 
# column to predict log(outbreak)
oldoutbreak <- train$outbreak
train$outbreak <- log10(train$outbreak + 1)

# Random Forest: 
ntrees <- seq(300, 700, by = 100)       # number of trees to create
mtrys <- seq(10, 60, by = 10)           # number of variables to consider
nodesizes <- seq(5, 20, by = 5)         # min number of obervations that have to be in a node 
maxnodes <- seq(5, 20, by = 5)          # maximum number of nodes in a given tree (minbuckets)

summary(train$outbreak)          # max should be ~2.62 (= log10(416)).
# If it's ~0.5, you've double-logged -> re-run cell 6, then cell 27 once.


set.seed(100)
param_grid_rf <- expand.grid(ntree    = ntrees,
                             mtry     = mtrys,
                             nodesize = nodesizes,
                             maxnodes = maxnodes)

results_rf_log <- data.frame()

for(i in 1:nrow(param_grid_rf)){
  fold_rmse       <- c()
  fold_mae        <- c()
  fold_r2         <- c()
  
  for(fold in folds){
    fold_train <- train[-fold, ]
    fold_test  <- train[fold, ]
    
    rf <- randomForest(outbreak ~ .,
                       data     = fold_train,
                       ntree    = param_grid_rf$ntree[i],
                       mtry     = param_grid_rf$mtry[i],
                       nodesize = param_grid_rf$nodesize[i],
                       maxnodes = param_grid_rf$maxnodes[i])
    
    predictions   <- predict(rf, newdata = fold_test)
    actual_values <- fold_test$outbreak
    
    fold_rmse <- c(fold_rmse, sqrt(mean((actual_values - predictions)^2)))
    fold_mae  <- c(fold_mae,  mean(abs(actual_values - predictions)))
    
    rss     <- sum((actual_values - predictions)^2)
    tss     <- sum((actual_values - mean(actual_values))^2)
    fold_r2 <- c(fold_r2, 1 - (rss / tss))
  }
  
  results_rf_log <- rbind(results_rf_log, data.frame(
    ntree    = param_grid_rf$ntree[i],
    mtry     = param_grid_rf$mtry[i],
    nodesize = param_grid_rf$nodesize[i],
    maxnodes = param_grid_rf$maxnodes[i],
    RMSE     = mean(fold_rmse),
    MAE      = mean(fold_mae),
    R2       = mean(fold_r2)
  ))
}


print(results_rf_log)

# FIRST ROW (FOR CHECKING REPRODUCIBILITY):
#ntree mtry nodesize maxnodes      RMSE       MAE         R2
#1     300   10        5        5 0.2874716 0.1479803 0.09024856



p_rf <- high(R2, df = results_rf_log) * low(RMSE) * low(MAE)
res_rf <- psel(results_rf_log, p_rf)

# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results_rf_log[, c("R2", "RMSE", "MAE")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results_rf_log[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res_rf)

# Draw perato Graph
# intersecting_points[nrow(intersecting_points) + 1, ] <- intersecting_points[1,]
res_rf <- intersecting_points[order(intersecting_points$MAE), ]

plot_ly(results_rf_log, x = ~R2, y = ~RMSE, z = ~MAE,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  add_trace(data = res_rf,
            x = ~R2, y = ~RMSE, z = ~MAE,
            type = "scatter3d", mode = "markers+lines",
            marker = list(color = "red", size = 6))

print(res_rf)
#ntree mtry nodesize maxnodes      RMSE       MAE        R2
#1   400   20        5       10 0.2835852 0.1448968 0.1116527
#3   600   30        5       10 0.2823460 0.1449875 0.1176721
#2   500   30        5       10 0.2822363 0.1452772 0.1091640

# FROM THIS WE CHOOSE ntree=600, mtry=30, nodesize=5, maxnodes=10

regress_rf_optparam <- res_rf[which(res_rf$R2==max(res_rf$R2)),]
print(regress_rf_optparam)

# Random Forest with best params 
set.seed(100)
rforest_model_final <- randomForest(
  outbreak ~ .,
  data     = train,
  ntree    = regress_rf_optparam$ntree, 
  mtry     = regress_rf_optparam$mtry, 
  nodesize = regress_rf_optparam$nodesize, 
  maxnodes = regress_rf_optparam$maxnodes, 
  importance = TRUE
)

oldtestoutbreak <- test$outbreak
actual    <- log10(test$outbreak + 1)
test$outbreak <- log10(test$outbreak + 1)

pred_rf   <- predict(rforest_model_final, newdata = test)
rf_rmse   <- sqrt(mean((actual - pred_rf)^2))
rf_mae    <- mean(abs(actual - pred_rf))
rf_r2     <- 1 - sum((actual - pred_rf)^2) / sum((actual - mean(actual))^2)
cat("Final Random Forest | RMSE:", round(rf_rmse,4), "MAE:", round(rf_mae,4), "R2:", round(rf_r2,4), "\n")

#Final Random Forest | RMSE: 0.3191 MAE: 0.1768 R2: 0.181  

imp <- importance(rforest_model_final)
imp_sorted <- imp[order(imp[, "%IncMSE"], decreasing = TRUE), ]
print(imp_sorted)

#### TOP TEN:
#                                                                 %IncMSE       IncNodePurity
#density                                                         5.967705e+00  7.246652e-01
#enrollment                                                      5.689012e+00  5.579232e-01
#population                                                      4.820979e+00  6.652588e-01
#pct_college                                                     4.358573e+00  5.473947e-01
#ep_nohsdp                                                       4.255401e+00  7.897755e-01
#ep_otherrace                                                    3.781253e+00  2.136895e-01
#last_dentist_visit_within_the_past_5_years_pct_diff             3.262616e+00  2.540216e-01
#any_cancer_yes                                                  3.239913e+00  6.728133e-02
#heavy_drinking_females_no_pct_diff                              3.204071e+00  2.068842e-01
#sigm_past_5_yrs_age_50_75_no                                    3.186426e+00  6.296254e-02


saveRDS(rforest_model_final, "results/regress_rf_model.rds")
write.csv(imp_sorted, "results/regress_rf_important_vars.csv", row.names=FALSE)
write_csv(results_rf_log, "results/regress_rf_paramtune_results.csv")
write_csv(res_rf, "results/regress_rf_best_combos.csv")
write_csv(regress_rf_optparam, "results/regress_rf_optparam.csv")


# Restore outbreak to its correct values at the end if needed later.
train$outbreak <- oldoutbreak
test$outbreak <- oldtestoutbreak


