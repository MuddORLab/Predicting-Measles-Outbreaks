rm(list=ls())
test <-read_csv("intermed_class_data/testclass.csv")
train <-read_csv("intermed_class_data/trainclass_smote.csv")
folds <-readRDS("intermed_class_data/folds_class.rds")

train$outbreak <- as.factor(as.numeric(train$outbreak))
test$outbreak <- as.factor(as.numeric(test$outbreak))


# random forest related
mtrys   <- c(seq(5, 50, by=5))
c_rf <- c(seq(0.2, 0.5, by=0.1))

# Parameter Tuning
set.seed(4)


results_rf <- data.frame()

for(m in mtrys){
  for(c_val in c_rf){
    fold_precision <- c()
    fold_recall <- c()
    fold_accuracy <- c()
    
    for(fold in folds){
      fold_train <- train[-fold, ]
      fold_test  <- train[fold, ]
      
      rf <- randomForest(outbreak ~ .,
                         data   = fold_train,
                         mtry   = m,
                         ntree  = 500,
                         cutoff = c(c_val, 1-c_val))
      
      preds <- predict(rf, newdata = fold_test, type = "class")
      cm <- table(Actual=fold_test$outbreak, Predicted=preds)
      fold_precision <- c(fold_precision, cm[2,2] / sum(cm[,2]))
      fold_recall <- c(fold_recall, cm[2,2] / sum(cm[2,]))
      fold_accuracy <- c(fold_accuracy, sum(diag(cm)) / sum(cm))
    }
    
    results_rf <- rbind(results_rf, data.frame(
      mtry = m,
      cutoff = c_val,
      precision = mean(fold_precision, na.rm=TRUE),
      precision_std = sd(fold_precision, na.rm=TRUE),
      recall = mean(fold_recall, na.rm=TRUE),
      recall_std = sd(fold_recall, na.rm=TRUE),
      accuracy = mean(fold_accuracy, na.rm=TRUE),
      accuracy_std = sd(fold_accuracy, na.rm=TRUE)
    ))
  }
}

print(results_rf)
# First row
# mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std
# 1     5    0.2 0.9763399    0.03069469 0.9176471 0.06904634 0.9481513   0.03244479

# Best by recall
best_rf <- results_rf[which.max(results_rf$recall), ]
print(best_rf)


# mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std
# 12   15    0.5 0.9554459    0.03426694 0.9764706 0.04112976 0.9654622   0.02246998

p_rf <- high(precision, df = results_rf) * high(recall) * high(accuracy)
res_rf <- psel(results_rf, p_rf)
print(res_rf)

#     mtry cutoff precision   precision_std    recall   recall_std  accuracy    accuracy_std
# 16   20     0.5 0.9610015    0.03670211     0.9764706 0.04112976 0.9684034   0.02499818
# 17   25     0.2 1.0000000    0.00000000     0.9235294 0.07362739 0.9625210   0.03582632
# 19   25     0.4 0.9780358    0.03771632     0.9705882 0.04159452 0.9742017   0.02502907
# 20   25     0.5 0.9616959    0.03627119     0.9764706 0.04112976 0.9683193   0.01617414
# 21   30     0.2 1.0000000    0.00000000     0.9235294 0.07867609 0.9625210   0.03827442
# 24   30     0.5 0.9610015    0.03670211     0.9764706 0.04112976 0.9684034   0.02499818
# 26   35     0.3 0.9944444    0.01756821     0.9647059 0.04960436 0.9799160   0.02355757
# 38   50     0.3 0.9944444    0.01756821     0.9647059 0.04960436 0.9799160   0.02355757

# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results_rf[, c("precision", "recall", "accuracy")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results_rf[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res_rf)
res_rf <- intersecting_points[order(intersecting_points$precision), ]
plot_ly(results_rf, x = ~precision, y = ~recall, z = ~accuracy,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  add_trace(data = res_rf,
            #alphahull = 0,
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


score_rf <- score_pareto(res_rf$precision, res_rf$recall, res_rf$accuracy, res_rf$precision_std, res_rf$recall_std, res_rf$accuracy_std)
res_rf$score <- score_rf
res_rf


#  mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std     score
# 1   20    0.5 0.9610015    0.03670211 0.9764706 0.04112976 0.9684034   0.02499818 0.7566234
# 3   25    0.5 0.9616959    0.03627119 0.9764706 0.04112976 0.9683193   0.01617414 0.7585204
# 2   25    0.4 0.9780358    0.03771632 0.9705882 0.04159452 0.9742017   0.02502907 0.7574268
# 5   35    0.3 0.9944444    0.01756821 0.9647059 0.04960436 0.9799160   0.02355757 0.7610839
# 4   30    0.2 1.0000000    0.00000000 0.9235294 0.07867609 0.9625210   0.03827442 0.7343028

# From this, mtry=35 and cutoff=0.3 seems like a good choice.


c = 0.3 #how many trees need to agree  .... was 0.4

set.seed(12345)
rforest_model <- randomForest(outbreak ~ ., data = train, ntree = 500, mtry = 35, importance = TRUE)

pred_rf <- predict(rforest_model, newdata = test, type = "class", cutoff=c(c, 1-c))
rforest_table <- table(Actual = test$outbreak, Predicted = pred_rf)
print(rforest_table)

# Calculate Precision, Accuracy, and recall
TP <- rforest_table[2, 2] # True Positives 
TN <- rforest_table[1, 1] # True Negatives 
FP <- rforest_table[1, 2] # False Positives 
FN <- rforest_table[2, 1] # False Negatives 

accuracy    <- (TP + TN) / sum(rforest_table) # total accuracy
recall <- TP / (TP + FN)                    # amount of positives guessed correctly out of all true positives
precision   <- TP / (TP + FP)               # amount of correct positive predictions

print(c(accuracy, recall,precision))

#> accuracy
# [1] 0.9193548
# > recall
# [1] 0.2
# > precision
# [1] 0.5

# Mean decreased accuracy basically says if a variable is omitted, how much does the mean accuracy decrease by
# Mean decreased gini measures how often the varaible is used to slip data into classes 
imp_matrix <- importance(rforest_model)
ordered_imp <- imp_matrix[order(imp_matrix[, 3], decreasing = TRUE), , drop = FALSE]

print(ordered_imp)


# True positives: 

# Random Forest
print("Random Forest True Positive:")
tp_rf <- test[test$outbreak == TRUE & pred_rf == TRUE, ]
tp_rf


# Random Forest
print("Random Forest False Negative:")
false_negatives_rf <- test[test$outbreak == TRUE & pred_rf == FALSE, ]
false_negatives_rf


# Random Forest
print("Random Forest False Positive:")
fp_rf <- test[test$outbreak == FALSE & pred_rf == TRUE, ]
fp_rf


# export FN results:
write_csv(false_negatives_rf, "results/class_rf_FNresults.csv")

#saving key variables that the rforest model uses
write.csv(ordered_imp, "results/class_rf_important_vars.csv")
write_csv(results_rf, "results/class_rf_paramtune_results.csv")
write_csv(res_rf, "results/class_rf_best_combos.csv")
saveRDS(rforest_model, "results/class_rf_model.rds")


