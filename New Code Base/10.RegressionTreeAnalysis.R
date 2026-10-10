rm(list=ls())
test <-read_csv("intermed_regress_data/test_clean.csv")
train <-read_csv("intermed_regress_data/train_clean.csv")
folds <-readRDS("intermed_regress_data/folds.rds")



# Log transform outbreak
# Need to store column separately because otherwise rpart uses original outbreak 
# column to predict log(outbreak)
oldoutbreak <- train$outbreak
train$outbreak <- log10(train$outbreak + 1)

# Base tree:
minsplits  <- seq(20, 50, by = 5)       #
minbuckets <- seq(10, 20, by = 5)       # min number of obervations that have to be in a node
maxdepths <- seq(10, 30, by = 10)       # max depth of the tree


summary(train$outbreak)          # max should be ~2.62 (= log10(416)).


set.seed(100)
param_grid_tree <- expand.grid(minsplit  = minsplits,
                               minbucket = minbuckets,
                               maxdepth  = maxdepths)
results_log <- data.frame()
for(i in 1:nrow(param_grid_tree)){
  fold_rmse       <- c()
  fold_mae        <- c()
  fold_r2         <- c()
  for(fold in folds){
    fold_train <- train[-fold, ]
    fold_test  <- train[fold, ]
    mod <- rpart(outbreak ~ .,
                 data    = fold_train,
                 method  = "anova",
                 control = rpart.control(
                   cp        = -1,
                   minsplit  = param_grid_tree$minsplit[i],
                   minbucket = param_grid_tree$minbucket[i],
                   maxdepth = param_grid_tree$maxdepth[i]
                 ))
    cp_table <- mod$cptable
    opt_cp   <- as.numeric(cp_table[which.min(cp_table[, "xerror"]), "CP"])[1]
    pruned   <- prune(mod, cp = opt_cp)
    predictions   <- predict(pruned, newdata = fold_test)
    actual_values <- fold_test$outbreak
    fold_rmse <- c(fold_rmse, sqrt(mean((actual_values - predictions)^2)))
    fold_mae  <- c(fold_mae,  mean(abs(actual_values - predictions)))
    rss     <- sum((actual_values - predictions)^2)
    tss     <- sum((actual_values - mean(actual_values))^2)
    fold_r2 <- c(fold_r2, 1 - (rss / tss))
  }
  results_log <- rbind(results_log, data.frame(
    minsplit  = param_grid_tree$minsplit[i],
    minbucket = param_grid_tree$minbucket[i],
    maxdepth = param_grid_tree$maxdepth[i],
    RMSE      = mean(fold_rmse),
    MAE       = mean(fold_mae),
    R2        = mean(fold_r2)
  ))
}

print(results_log)


##### IDENTIFYING BEST PARAMETER COMBINATIONS ######
p <- high(R2, df = results_log) * low(RMSE) * low(MAE)
res <- psel(results_log, p)

# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results_log[, c("R2", "RMSE", "MAE")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results_log[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res)

# Draw perato Graph
# intersecting_points[nrow(intersecting_points) + 1, ] <- intersecting_points[1,]
res <- intersecting_points[order(intersecting_points$MAE), ]

plot_ly(results_log, x = ~R2, y = ~RMSE, z = ~MAE,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  add_trace(data = res,
            x = ~R2, y = ~RMSE, z = ~MAE,
            type = "scatter3d", mode = "markers+lines",
            marker = list(color = "red", size = 6))

print(res)


# Regression Tree with best params
set.seed(100)
dtree_model_final <- rpart(
  outbreak ~ .,
  data    = train,
  method  = "anova",
  control = rpart.control(
    cp        = -1,
    maxdepth  = 30, # was showing as 30
    minsplit  = 50, # was showing as 20
    minbucket = 20 # was showing as 15
  )
)
opt_cp        <- as.numeric(dtree_model_final$cptable[which.min(dtree_model_final$cptable[, "xerror"]), "CP"])[1]
print(opt_cp)
# opt_cp=0.01971557

dtree_model_final <- prune(dtree_model_final, cp = opt_cp)

oldtestoutbreak <- test$outbreak
actual    <- log10(test$outbreak + 1)
test$outbreak <- log10(test$outbreak + 1)
pred_dt   <- predict(dtree_model_final, newdata = test)  # Only predicts a single value, not 62 different values
dt_rmse   <- sqrt(mean((actual - pred_dt)^2))
dt_mae    <- mean(abs(actual - pred_dt))
dt_r2     <- 1 - sum((actual - pred_dt)^2) / sum((actual - mean(actual))^2)
cat("Final Regression Tree | RMSE:", round(dt_rmse,4), "MAE:", round(dt_mae,4), "R2:", round(dt_r2,4), "\n")


# Final Regression Tree | RMSE: 0.3395 MAE: 0.1718 R2: 0.0728 

importance_df <- data.frame(
  Variable = names(dtree_model_final$variable.importance),
  Importance = dtree_model_final$variable.importance,
  row.names = NULL
)

print(importance_df)

# Variable Importance
# 1              ever_had_blood_stool_test_yes_pct_diff    3.02379
# 2                  heavy_drinking_females_no_pct_diff    3.02379
# 3 last_dentist_visit_within_the_past_2_years_pct_diff    3.02379
# 4 last_dentist_visit_within_the_past_5_years_pct_diff    3.02379
# 5                                 asthma_status_never    2.49018
# 6                              bmi_3_categories_obese    2.49018


write.csv(importance_df, "results/regress_tree_important_vars.csv")
saveRDS(dtree_model_final, "results/regress_tree_model.rds")
write.csv(results_log, "results/regress_tree_paramtune_results.csv")
write.csv(res, "results/regress_tree_best_combos.csv")


# Restore outbreak to its correct values at the end if needed later.
train$outbreak <- oldoutbreak
test$outbreak <- oldtestoutbreak


























