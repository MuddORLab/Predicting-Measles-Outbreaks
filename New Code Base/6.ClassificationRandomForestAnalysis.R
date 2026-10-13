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
#     mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std
# 1     5    0.2 0.9823529    0.03970286 0.8992647 0.06845654 0.9416705   0.03670114


p_rf <- high(precision, df = results_rf) * high(recall) * high(accuracy)
res_rf <- psel(results_rf, p_rf)
print(res_rf)

#     mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std
# 9    15    0.2 1.0000000    0.00000000 0.9102941 0.07008261 0.9563764   0.03397308
# 14   20    0.3 0.9835913    0.03628299 0.9525735 0.05415641 0.9680621   0.02517545
# 15   20    0.4 0.9780358    0.03771632 0.9702206 0.03140939 0.9737764   0.01665710
# 17   25    0.2 0.9933333    0.02108185 0.9161765 0.07524151 0.9563764   0.03922547
# 19   25    0.4 0.9780358    0.03771632 0.9702206 0.03140939 0.9737764   0.01665710
# 23   30    0.4 0.9780358    0.03771632 0.9702206 0.03140939 0.9737764   0.01665710
# 32   40    0.5 0.9665979    0.03803005 0.9764706 0.03037634 0.9710924   0.01933463
# 37   50    0.2 0.9933333    0.02108185 0.9161765 0.05791786 0.9563764   0.03153443

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
print(res_rf)


#    mtry cutoff precision precision_std    recall recall_std  accuracy accuracy_std     score
# 4   40    0.5 0.9665979    0.03803005 0.9764706 0.03037634 0.9710924   0.01933463 0.7610194
# 3   20    0.4 0.9780358    0.03771632 0.9702206 0.03140939 0.9737764   0.01665710 0.7608631
# 2   20    0.3 0.9835913    0.03628299 0.9525735 0.05415641 0.9680621   0.02517545 0.7466054
# 5   50    0.2 0.9933333    0.02108185 0.9161765 0.05791786 0.9563764   0.03153443 0.7301229
# 1   15    0.2 1.0000000    0.00000000 0.9102941 0.07008261 0.9563764   0.03397308 0.7295638

# From this, mtry=40 and cutoff=0.5 seems like a good choice.


c = 0.5 #how many trees need to agree  .... was 0.4

set.seed(12345)
rforest_model <- randomForest(outbreak ~ ., data = train, ntree = 500, mtry = 40, importance = TRUE)

pred_rf <- predict(rforest_model, newdata = test, type = "class", cutoff=c(c, 1-c))
rforest_table <- table(Actual = test$outbreak, Predicted = pred_rf)
print(rforest_table)


### Confusion Matrix
#         Predicted
# Actual  0  1
# 0       57  1
#1        2  1

# Calculate Precision, Accuracy, and recall
TP <- rforest_table[2, 2] # True Positives 
TN <- rforest_table[1, 1] # True Negatives 
FP <- rforest_table[1, 2] # False Positives 
FN <- rforest_table[2, 1] # False Negatives 

accuracy    <- (TP + TN) / sum(rforest_table) # total accuracy
recall <- TP / (TP + FN)                    # amount of positives guessed correctly out of all true positives
precision   <- TP / (TP + FP)               # amount of correct positive predictions

print(c(accuracy, recall,precision))

#print(c(accuracy, recall,precision))
#[1] 0.9508197 0.3333333 0.5000000

# Mean decreased accuracy basically says if a variable is omitted, how much does the mean accuracy decrease by
# Mean decreased gini measures how often the varaible is used to slip data into classes 
imp_matrix <- importance(rforest_model)
ordered_imp <- imp_matrix[order(imp_matrix[, 3], decreasing = TRUE), , drop = FALSE]

print(ordered_imp)
### HERE ARE THE FIRST TEN ENTRIES
#                                                                   0           1           MeanDecreaseAccuracy MeanDecreaseGini
# enrollment                                                     11.7919995 15.78460303           15.8385091     9.2610256833
# ep_otherrace                                                    6.4546121 15.73276454           15.5145896     6.0480981987
# population                                                     11.1914379 15.28244781           15.2666515     8.4398835980
# density                                                        11.1393112 13.97066361           14.2221618     8.0500784681
# ep_aian                                                         2.0817981 11.05798569           11.0119743     4.2756615808
# ep_hburd                                                        2.1665255 11.02566765           10.9077672     2.5946194725
# ep_asian                                                        4.7819319  9.07238140            9.5576876     1.4480081200
# pct_uninsured                                                  -0.7446758  9.52923827            9.2304535     1.1888482790
# overweight_or_obese_yes_pct_diff                                8.5552401  7.96408717            9.1951185     8.0925136551
# ep_munit                                                        1.7082544  9.17924384            8.9789406     2.0898331480



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


