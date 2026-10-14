rm(list=ls())
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
#1     300   10        5        5 0.2890557 0.1484703 0.07553131



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
#     ntree mtry nodesize maxnodes      RMSE       MAE        R2
# 4     600   40        5        5 0.2843012 0.1450026 0.1084696
# 2     300   50        5       10 0.2834105 0.1450278 0.1092629
# 3     500   30        5       10 0.2819236 0.1453789 0.1104423
# 1     300   20       10       15 0.2834374 0.1481219 0.1147234

# FROM THIS WE CHOOSE ntree=300, mtry=20, nodesize=10, maxnodes=15

# Random Forest with best params 
set.seed(100)
rforest_model_final <- randomForest(
  outbreak ~ .,
  data     = train,
  ntree    = 300, # was showing 700, which doesn't match paper
  mtry     = 20, # was showing 20
  nodesize = 10, # was showing 5
  maxnodes = 15, # was showing 10
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

#Final Random Forest | RMSE: 0.3102 MAE: 0.1742 R2: 0.226 

imp <- importance(rforest_model_final)
imp_sorted <- imp[order(imp[, "%IncMSE"], decreasing = TRUE), ]
print(imp_sorted)

#### TOP TEN:
#                                                                 %IncMSE       IncNodePurity
# enrollment                                                      4.925893e+00  6.167089e-01
# density                                                         4.285889e+00  4.445293e-01
# population                                                      3.972075e+00  5.888501e-01
# ep_nohsdp                                                       3.745808e+00  9.306195e-01
# ep_age65                                                        3.673960e+00  6.503291e-01
# ep_hburd                                                        3.371923e+00  5.863674e-01
# any_cancer_yes                                                  2.506315e+00  1.149544e-01
# pct_college                                                     2.485825e+00  2.603315e-01
# mammogram_yes                                                   2.337985e+00  5.397563e-02
# obese_yes_pct_diff                                              2.202661e+00  7.006925e-02


saveRDS(rforest_model_final, "results/regress_rf_model.rds")
write.csv(imp_sorted, "results/regress_rf_important_vars.csv", row.names=FALSE)
write_csv(results_rf_log, "results/regress_rf_paramtune_results.csv")
write_csv(res_rf, "results/regress_rf_best_combos.csv")


# Restore outbreak to its correct values at the end if needed later.
train$outbreak <- oldoutbreak
test$outbreak <- oldtestoutbreak


