rm(list=ls())
test <-read_csv("intermed_class_data/testclass.csv")
train <-read_csv("intermed_class_data/trainclass_smote.csv")
folds <-readRDS("intermed_class_data/folds_class.rds")

# XGBoost related
param_grid <- expand.grid( # tuning grid
  max_depth = c(seq(3, 9, by=2)),
  eta = c(0.01,seq(0.05, 0.4, by=0.05)),
  cutoff = c(seq(0.2, 0.5, by=0.1)) # deciding what counts as outbreak
)

train$outbreak <- as.numeric(train$outbreak)
test$outbreak <- as.numeric(test$outbreak)


set.seed(4)
#foldsjunk <- createFolds(train$outbreak, k=10)

results_xgb <- data.frame()

for(i in 1:nrow(param_grid)){
  fold_precision <- c()
  fold_recall    <- c()
  fold_accuracy  <- c()
  
  for(fold in folds){
    fold_train <- train[-fold, ]
    fold_test  <- train[fold, ]
    
    X_fold_train <- as.matrix(fold_train[, names(fold_train) != "outbreak"])
    #y_fold_train <- as.numeric(as.character(fold_train$outbreak))
    y_fold_train <- fold_train$outbreak
    X_fold_test  <- as.matrix(fold_test[, names(fold_test) != "outbreak"])
    y_fold_test <- fold_test$outbreak
    #y_fold_test  <- as.numeric(as.character(fold_test$outbreak))
    
    dtrain_fold  <- xgb.DMatrix(data = X_fold_train, label = y_fold_train)
    dtest_fold   <- xgb.DMatrix(data = X_fold_test,  label = y_fold_test)
    model <- xgb.train(
      params = list(
        objective   = "binary:logistic",
        eval_metric = "logloss",
        max_depth   = param_grid$max_depth[i],
        eta         = param_grid$eta[i]
      ),
      data    = dtrain_fold,
      nrounds = 500
    )
    
    preds <- as.numeric(predict(model, dtest_fold) >= param_grid$cutoff[i])
    
    cm <- table(
      factor(fold_test$outbreak, levels = c(0, 1)),
      factor(preds,              levels = c(0, 1))
    )
    
    tp <- cm[2, 2]
    fp <- cm[1, 2]
    fn <- cm[2, 1]
    
    precision <- if((tp + fp) == 0) NA else tp / (tp + fp)
    recall    <- if((tp + fn) == 0) NA else tp / (tp + fn)
    accuracy  <- sum(diag(cm)) / sum(cm)
    
    fold_precision <- c(fold_precision, precision)
    fold_recall    <- c(fold_recall,    recall)
    fold_accuracy  <- c(fold_accuracy,  accuracy)
  }
  
  results_xgb <- rbind(results_xgb, data.frame(
    max_depth     = param_grid$max_depth[i],
    eta           = param_grid$eta[i],
    cutoff        = param_grid$cutoff[i],
    precision     = mean(fold_precision, na.rm = TRUE),
    precision_std = sd(fold_precision,   na.rm = TRUE),
    recall        = mean(fold_recall,    na.rm = TRUE),
    recall_std    = sd(fold_recall,      na.rm = TRUE),
    accuracy      = mean(fold_accuracy,  na.rm = TRUE),
    accuracy_std  = sd(fold_accuracy,    na.rm = TRUE)
  ))
}

print(results_xgb)


p_xgb <- high(precision, df = results_xgb) * high(recall) * high(accuracy)
res_xgb <- psel(results_xgb, p_xgb)


# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results_xgb[, c("precision", "recall", "accuracy")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results_xgb[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res_xgb)

# for single point case, add a dummy point to make it plottable
#intersecting_points[nrow(intersecting_points) + 1, ] <- intersecting_points[1,]
res_xgb <- intersecting_points[order(intersecting_points$precision), ]


plot_ly(results_xgb, x = ~precision, y = ~recall, z = ~accuracy,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  add_trace(data = res_xgb,
            x = ~precision, y = ~recall, z = ~accuracy,
            type = "scatter3d", mode = "markers+lines",
            marker = list(color = "red", size = 6))

# score func
score_pareto <- function(prec, rec, acc, precsd, recsd, accsd) {
  beta = 2
  w1 = 0.5
  w2 = 0.3
  w3 = 0.2
  f_beta = (1 + beta^2) * (prec * rec) / (beta^2 * prec + rec + 1e-6)
  score = w1 * f_beta + w2 * acc - w3 * (precsd + recsd + accsd)
  return(score)
}


score_xgb <- score_pareto(res_xgb$precision, res_xgb$recall, res_xgb$accuracy, res_xgb$precision_std, res_xgb$recall_std, res_xgb$accuracy_std) 
res_xgb$score <- score_xgb
print(res_xgb)


#   max_depth  eta cutoff precision precision_std    recall recall_std  accuracy accuracy_std     score
# 2         3 0.40    0.2 0.9406089    0.05221518 0.9941176 0.01860163 0.9647746   0.02750661 0.7612348
# 4         7 0.35    0.2 0.9501892    0.03930362 0.9882353 0.02480218 0.9680621   0.02126964 0.7635356
# 3         5 0.30    0.4 0.9675095    0.04490848 0.9823529 0.02841446 0.9739496   0.02139147 0.7629159
# 1         3 0.15    0.5 0.9774510    0.02912594 0.9764706 0.04112976 0.9768908   0.01816306 0.7637166

# boos: The algorithm draws a bootstrap sample of the training data in each iteration using the weights for each observation. 
# Misclassified or harder-to-predict samples receive higher weights, meaning they are 
# more likely to be selected for the next tree's training data

# mfinal: number of iterations

# FROM THIS WE CHOOSE MAX_DEPTH = 3; ETA=0.15 AND CUTOFF=0.5
cutoff = 0.5

# Prepare matrices for xgb boost
X_train <- as.matrix(train[, names(train) != "outbreak"])
y_train <- train$outbreak
#y_train <- as.numeric(as.character(train$outbreak))

X_test  <- as.matrix(test[, names(test) != "outbreak"])
y_test <- test$outbreak
#y_test  <- as.numeric(as.character(test$outbreak))

dtrain  <- xgb.DMatrix(data = X_train, label = y_train)
dtest   <- xgb.DMatrix(data = X_test,  label = y_test)

boost_model <- xgb.train(
  params = list(
    objective   = "binary:logistic",
    eval_metric = "logloss",
    max_depth   = 3, # WAS 7
    eta         = 0.15
  ),
  data    = dtrain,
  nrounds = 500
)

pred_xgb <- as.numeric(predict(boost_model, dtest)>= cutoff)

boost_table <- table(Actual = y_test, Predicted = pred_xgb)
print(boost_table)

# Calculate Precision, Accuracy, and recall
TP <- boost_table[2, 2] # True Positives 
TN <- boost_table[1, 1] # True Negatives 
FP <- boost_table[1, 2] # False Positives 
FN <- boost_table[2, 1] # False Negatives 

accuracy    <- (TP + TN) / sum(boost_table) # total accuracy
recall <- TP / (TP + FN)                    # amount of positives guessed correctly out of all true positives
precision   <- TP / (TP + FP)               # amount of correct positive predictions

print(c(accuracy, recall, precision))

# importance graph
importance_matrix <- xgb.importance(model = boost_model)
# xgb.plot.importance(importance_matrix)
importance_matrix$Feature

#saving variables models used
print(importance_matrix)



print("Boost:")
# Xgboost 
tp_xgb <- test[y_test == TRUE & pred_xgb == TRUE, ]
print(tp_xgb)

# False negative: 

# Xgboost 
false_negatives_xgb <- test[y_test == TRUE & pred_xgb == FALSE, ]
print(false_negatives_xgb)

# False positives: 

# Xgboost 
fp_xgb <- test[y_test == FALSE & pred_xgb == TRUE, ]
print(fp_xgb)

# export FN results (merged_with_svi):
write_csv(false_negatives_xgb, "results/class_xgb_FNresults.csv")
write.csv(importance_matrix, "results/class_xgb_important_vars.csv")
saveRDS(boost_model, "results/class_xgb_model.rds")
write_csv(results_xgb, "results/class_xgb_paramtune_results.csv")
write_csv(res_xgb, "results/class_xgb_best_combos.csv")

