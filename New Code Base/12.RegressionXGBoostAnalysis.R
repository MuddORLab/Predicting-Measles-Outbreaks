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


# Xgboost 
eta <- c(0.01, seq(0.05, 0.4, by = 0.05))  # learning rate of each tree
max_depth <- seq(3, 9, by = 3)             # max depth of any tree
min_child_weight <- seq(1, 5, by = 1)      # min nunmber of observations that have to be in a node (minbuckets)
subsample <- seq(0.4, 1, by = 0.1)         # samples a subsample of the total data for each tree
early_stopping <- seq(10, 30, by = 10)     # number of iterations where model stops training if there is marginal imporvement   


summary(train$outbreak)          # max should be ~2.62 (= log10(416)).
# If it's ~0.5, you've double-logged -> re-run cell 6, then cell 27 once.


set.seed(100)
param_grid_xgb <- expand.grid(
  eta               = eta,
  max_depth         = max_depth,
  min_child_weight  = min_child_weight,
  subsample         = subsample,
  early_stopping_rounds    = early_stopping
)

results_xgb_log <- data.frame()

for (i in 1:nrow(param_grid_xgb)) {
  fold_rmse       <- c()
  fold_mae        <- c()
  fold_r2         <- c()
  
  for (fold in folds) {
    fold_train <- train[-fold, ]
    fold_test  <- train[fold, ]
    
    dtrain <- xgb.DMatrix(
      data  = as.matrix(fold_train[, !names(fold_train) %in% "outbreak"]),
      label = fold_train$outbreak
    )
    dtest <- xgb.DMatrix(
      data  = as.matrix(fold_test[, !names(fold_test) %in% "outbreak"]),
      label = fold_test$outbreak
    )
    
    xgb_model <- xgb.train(
      params = list(
        objective        = "reg:squarederror",
        eta              = param_grid_xgb$eta[i],
        max_depth        = param_grid_xgb$max_depth[i],
        min_child_weight = param_grid_xgb$min_child_weight[i],
        subsample        = param_grid_xgb$subsample[i]
      ),
      data       = dtrain,
      nrounds    = 500,
      evals      = list(train = dtrain, eval = dtest),  
      early_stopping_rounds = param_grid_xgb$early_stopping_rounds[i],
      verbose    = 0
    )
    
    predictions   <- predict(
      xgb_model, dtest,
      iteration_range = c(1, xgb_model$best_iteration) 
    )
    actual_values <- fold_test$outbreak
    
    fold_rmse <- c(fold_rmse, sqrt(mean((actual_values - predictions)^2)))
    fold_mae  <- c(fold_mae,  mean(abs(actual_values - predictions)))
    
    rss     <- sum((actual_values - predictions)^2)
    tss     <- sum((actual_values - mean(actual_values))^2)
    fold_r2 <- c(fold_r2, 1 - (rss / tss))
  }
  
  results_xgb_log <- rbind(results_xgb_log, data.frame(
    eta              = param_grid_xgb$eta[i],
    max_depth        = param_grid_xgb$max_depth[i],
    min_child_weight = param_grid_xgb$min_child_weight[i],
    subsample        = param_grid_xgb$subsample[i],
    early_stopping   = param_grid_xgb$early_stopping_rounds[i],
    RMSE             = mean(fold_rmse),
    MAE              = mean(fold_mae),
    R2               = mean(fold_r2)
  ))
}

print(results_xgb_log)

# FIRST ROW FOR REPRODUCIBILITY:
#     eta max_depth min_child_weight subsample early_stopping      RMSE       MAE            R2
# 1   0.01         3                1       0.4             10 0.2940506 0.1621077  0.0553421829



p_xgb <- high(R2, df = results_xgb_log) * low(RMSE) * low(MAE)
res_xgb <- psel(results_xgb_log, p_xgb)

# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results_xgb_log[, c("R2", "RMSE", "MAE")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results_xgb_log[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res_xgb)

# Draw perato Graph
# intersecting_points[nrow(intersecting_points) + 1, ] <- intersecting_points[1,]
res_xgb <- intersecting_points[order(intersecting_points$MAE), ]

plot_ly(results_xgb_log, x = ~R2, y = ~RMSE, z = ~MAE,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  add_trace(data = res_xgb,
            x = ~R2, y = ~RMSE, z = ~MAE,
            type = "scatter3d", mode = "markers+lines",
            marker = list(color = "red", size = 6))

print(res_xgb)

#   eta   max_depth   min_child_weight subsample early_stopping      RMSE       MAE        R2
# 5 0.40         9                1       0.9             30 0.2601933 0.1229986 0.2466984
# 3 0.30         3                1       0.9             10 0.2603764 0.1273998 0.2701164
# 1 0.20         6                1       0.9             20 0.2526352 0.1297055 0.3192401
# 2 0.20         9                1       0.9             10 0.2500520 0.1300503 0.3194337
# 4 0.35         3                4       0.5             30 0.2459714 0.1335015 0.3502361


# FROM THIS, ETA=0.35, MAX_DEPTH=3, MIN_CHILD_WEIGHT=4, SUBSAMPLE=0.5 AND EARLY_STOPPING=30 SEEM LIKE GOOD CHOICES.
# XGBoost with best params
set.seed(100)

# split train: 80% to train on (tr), 20% held out for early-stopping validation (val)
val_idx <- sample(seq_len(nrow(train)), floor(0.2 * nrow(train)))
tr  <- train[-val_idx, ]
val <- train[ val_idx, ]

dtr  <- xgb.DMatrix(as.matrix(tr[,  names(tr)  != "outbreak"]), label = tr$outbreak)   # trains the model
dval <- xgb.DMatrix(as.matrix(val[, names(val) != "outbreak"]), label = val$outbreak)  # early stopping watches this

y_test_final <- log10(test$outbreak + 1)
dtest_final  <- xgb.DMatrix(as.matrix(test[, names(test) != "outbreak"]), label = y_test_final)

# ETA=0.35, MAX_DEPTH=3, MIN_CHILD_WEIGHT=4, SUBSAMPLE=0.5 AND EARLY_STOPPING=30 SEEM LIKE GOOD CHOICES.

boost_model_final <- xgb.train(
  params = list(
    objective        = "reg:squarederror",
    eval_metric      = "rmse",
    eta              = 0.35,
    max_depth        = 3, # was showing 9
    min_child_weight = 4, # waas showing 1
    subsample        = 0.5 # was showing 0.8
  ),
  data    = dtr,                                # train on the 80% subset
  nrounds = 500,
  evals   = list(train = dtr, eval = dval),     # dval LAST; test set is NOT here
  early_stopping_rounds = 30,
  verbose = 0
)

pred_xgb <- predict(boost_model_final, dtest_final,
                    iteration_range = c(1, boost_model_final$best_iteration))
xgb_rmse <- sqrt(mean((y_test_final - pred_xgb)^2))
xgb_mae  <- mean(abs(y_test_final - pred_xgb))
xgb_r2   <- 1 - sum((y_test_final - pred_xgb)^2) / sum((y_test_final - mean(y_test_final))^2)
cat("Final XGBoost | RMSE:", round(xgb_rmse,4), "MAE:", round(xgb_mae,4), "R2:", round(xgb_r2,4), "\n")


# Final XGBoost | RMSE: 0.3818 MAE: 0.2414 R2: -0.1722 

print(xgb.importance(model = boost_model_final))

# TOP ELEVEN:
#                                       Feature         Gain        Cover   Frequency
#                                       <char>        <num>        <num>       <num>
#   1:                                pct_college 0.1821883920 0.0636523673 0.053398058
# 2:                                   ep_age65 0.0803262039 0.1208190494 0.101941748
# 3:                                   ep_hburd 0.0773968640 0.0574446400 0.053398058
# 4:                                 enrollment 0.0756406186 0.0299268044 0.038834951
# 5:                              pct_uninsured 0.0634920125 0.0226072454 0.019417476
# 6:                                        cve 0.0588418251 0.0506809969 0.048543689
# 7:                                   ep_age17 0.0541488016 0.0721764106 0.063106796
# 8:                                  ep_disabl 0.0468472848 0.0622625776 0.067961165
# 9:                                   ep_munit 0.0457708048 0.0687482628 0.067961165
# 10:                  served_on_active_duty_yes 0.0373961320 0.0008338738 0.004854369
# 11:                                  ep_nohsdp 0.0369399852 0.0428981747 0.038834951


temp <- xgb.importance(model = boost_model_final)
plot(temp$Gain)


write_csv(results_xgb_log, "results/regress_xgb_paramtune_results.csv")
saveRDS(boost_model_final, "results/regress_xgb_model.rds")
write.csv(xgb.importance(model = boost_model_final), "results/regress_xgb_important_vars.csv", row.names=FALSE)
write_csv(res_xgb, "results/regress_xgb_best_combos.csv")


# Restore outbreak to its correct values at the end if needed later.
train$outbreak <- oldoutbreak


