# SET WORKING DIRECTORY TO THE DIRECTORY WHERE THIS FILE IS STORED.
rm(list=ls())
source("1.LoadLibraries.R")


test <-read_csv("intermed_class_data/testclass.csv")
train <-read_csv("intermed_class_data/trainclass_smote.csv")
folds <-readRDS("intermed_class_data/folds_class.rds")

FNlosses <- c(seq(1, 10, by=0.1))
cp0 <- -1


# Parameter Tuning
set.seed(4)
results <- data.frame()

for(j in 1:length(FNlosses)){
  fold_precision <- c()
  fold_recall    <- c()
  fold_accuracy  <- c()
  fold_cp <- c() 
  
  for(fold in folds){
    fold_train <- train[-fold, ]
    fold_test  <- train[fold, ]
    
    mod <- rpart(outbreak ~ .,
                 data   = fold_train,
                 method = "class",
                 parms  = list(loss=matrix(c(0,FNlosses[j],1,0), nrow=2)),
                 #weights= ceiling(fold_train$population),
                 cp     = cp0) 
    
    cp_table <- mod$cptable
    opt_cp <- cp_table[which.min(cp_table[,"xerror"]), "CP"]
    pruned <- prune(mod, cp=opt_cp)
    
    preds <- predict(pruned, newdata=fold_test, type="class")
    cm    <- table(Actual=fold_test$outbreak, Predicted=preds)
    
    # precision = TP/(TP + FP)
    fold_precision <- c(fold_precision, cm[2,2]/sum(cm[,2]))
    # recall = TP/(TP + FN)
    fold_recall <- c(fold_recall, cm[2,2]/sum(cm[2,]))
    # accuracy = correct predictions/total
    fold_accuracy <- c(fold_accuracy, sum(diag(cm))/sum(cm))
    fold_cp <- c(fold_cp, opt_cp)   
  }
  
  results <- rbind(results, data.frame(
    FNloss        = FNlosses[j],
    cp            = mean(fold_cp),                
    cp_std        = sd(fold_cp), 
    precision     = mean(fold_precision, na.rm=TRUE),
    precision_std = sd(fold_precision, na.rm=TRUE),
    recall        = mean(fold_recall,    na.rm=TRUE),
    recall_std    = sd(fold_recall,      na.rm=TRUE),
    accuracy      = mean(fold_accuracy,  na.rm=TRUE),
    accuracy_std  = sd(fold_accuracy,    na.rm=TRUE)
  ))
}

print(results)
# First row of results:
#FNloss           cp       cp_std precision precision_std    recall recall_std  accuracy accuracy_std
#1              1.0 0.0043046358 0.0114758444 0.8903782    0.05843220 0.9404412 0.05636722 0.9126687   0.04906392

p <- high(precision, df = results) * high(recall) * high(accuracy)
res <- psel(results, p)

# Create the actual convex hull for the full results vector
hull_mat <- as.matrix(results[, c("precision", "recall", "accuracy")])
hull_idx <- convhulln(hull_mat) 

# hull_idx is a dataframe of triangles (surface) that encapsulate all points
# find only the unique values so they all connect
unique_indices <- unique(as.vector(hull_idx))
results_hull <- results[unique_indices, ]

# Take only intersecting points of the data
intersecting_points <- merge(results_hull, res)
res <- intersecting_points[order(intersecting_points$precision), ]

# Plot all points
plot_ly(results, x = ~precision, y = ~recall, z = ~accuracy,
        type = "scatter3d", mode = "markers",
        marker = list(color = "blue", size = 4))%>%
  # Overlay Pareto pts
  add_trace(data = res,
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

score_ctree <- score_pareto(res$precision, res$recall, res$accuracy, res$precision_std, res$recall_std, res$accuracy_std)
res$score <- score_ctree

print(res)

#FNloss         cp     cp_std precision precision_std    recall recall_std  accuracy accuracy_std     score
#1    1.7 0.00443038 0.01401009  0.907807     0.0501763 0.9823529 0.03970286 0.9421747    0.0402898 0.7398586

# FROM THIS WE CHOOSE LOSS LAMBDA = 1.7

set.seed(91711)
# set decision tree
ctree_model <- rpart(outbreak ~ ., data = train, method = "class", 
                     parms = list(loss = matrix(c(0, 3.1, 1, 0), nrow = 2)))
opt_cp        <- as.numeric(ctree_model$cptable[which.min(ctree_model$cptable[, "xerror"]), "CP"])[1]
ctree_model <- prune(ctree_model, cp = opt_cp)
print(opt_cp)
# 0.01647727

# eval performance
pred_dt <- predict(ctree_model, newdata = test, type = "class")



### PLOT DECISION TREE
print(ctree_model$frame$var)

# [1] "overweight_or_obese_yes_pct_diff" "<leaf>"                           "enrollment"                      
#[4] "<leaf>"                           "pct_black"                        "<leaf>"                          
#[7] "ep_otherrace"                     "<leaf>"                           "<leaf>"  

var_descriptions <- c(
  "overweight_or_obese_yes_pct_diff" = "chg from 2014 in pct of people overweight or obese",
  "pct_black" = "pct of people who are Black",
  "ep_otherrace" = "est pct of people who are Other Race",
  "enrollment" = "school enrollments"
)

ctree_model$frame$var <- ifelse(
  ctree_model$frame$var %in% names(var_descriptions),
  var_descriptions[ctree_model$frame$var],
  as.character(ctree_model$frame$var)
)

node_vars <- as.character(ctree_model$frame$var)
print(node_vars)

var_colors <- c(
  "overweight_or_obese_yes_pct_diff" = "skyblue",
  "pct_black" = "salmon",
  "enrollment" = "gold",
  "ep_otherrace"="purple"
)

mapped_var_colors <- var_colors
names(mapped_var_colors) <- var_descriptions[names(var_colors)]

node_colors <- ifelse(node_vars == "<leaf>", "gray90",
                      ifelse(node_vars %in% names(mapped_var_colors), mapped_var_colors[node_vars], "white"))

# wrap long descriptions onto multiple lines so they fit the legend panel
# instead of running off the edge -- keeps full text, just breaks it across lines
legend_labels <- sapply(var_descriptions[names(var_colors)], function(txt) {
  paste(strwrap(txt, width = 22), collapse = "\n")
})

# split the plotting device into 2 separate panels: tree gets ~58% of the width,
# legend gets ~42% -- they live in physically distinct regions, so overlap is impossible
png("results/class_tree_plot.png", width = 8, height = 6, units = "in", res = 300)

layout(matrix(c(1, 2), nrow = 1), widths =c(0.7, 0.3))

# panel 1: the tree, unchanged
# par(mar = c(bottom, left, top, right)) -> default is c(5, 4, 4, 2) + 0.1
# We increase the left margin from 4 to 6 or 7
#par(mar = c(5, 7, 4, 2)) 
par(mar = c(5,4,4,2))
rpart.plot(ctree_model, box.col = node_colors, tweak=0.7)


# panel 2: blank canvas dedicated entirely to the legend
par(mar = c(1, 0.5, 1, 0.5))
plot.new()
legend("left",
       legend = legend_labels,
       fill   = var_colors,
       title  = "Split Variable",
       cex    = 0.72,
       bty    = "n",
       y.intersp = 1.8)   # extra vertical spacing since labels now span multiple lines

dev.off()


# CONFUSION MATRIX

ctree_table <- table(Actual = test$outbreak, Predicted = pred_dt)
print(ctree_table)

#         Predicted
# Actual  FALSE TRUE
# FALSE    55    3
# TRUE      2    1

# Calculate Precision, Accuracy, and recall
TP <- ctree_table[2, 2] # True Positives 
TN <- ctree_table[1, 1] # True Negatives 
FP <- ctree_table[1, 2] # False Positives 
FN <- ctree_table[2, 1] # False Negatives 

accuracy    <- (TP + TN) / sum(ctree_table) # total accuracy
recall <- TP / (TP + FN)                    # amount of positives guessed correctly out of all true positives
precision   <- TP / (TP + FP)               # amount of correct positive predictions

print(c(accuracy, recall, precision))

# [1] 0.9180328 0.3333333 0.2500000

#### WARNING ABOUT VARIABLE.IMPORTANCE. IT USES THE FULL TREE, NOT THE PRUNED TREE.
#### HIGH IMPORTANCE VARIABLES MIGHT NOT APPEAR IN THE PRUNED TREE DUE TO 
#### SURROGATE SPLITS OR COLLINEARITY.
importance_df <- data.frame(
  Variable = names(ctree_model$variable.importance),
  Importance = ctree_model$variable.importance,
  row.names = NULL
)

print(importance_df)

## HERE ARE THE TOP TEN:
#         Variable Importance
#1    overweight_or_obese_yes_pct_diff 62.9858439
#2  bmi_3_categories_recommended_range 55.0361743
#3                  current_smoker_yes 55.0361743
#4             overweight_or_obese_yes 55.0361743
#5  bmi_4_categories_recommended_range 53.8131482
#6  bmi_5_categories_recommended_range 53.8131482
#7                          enrollment 31.5580529
#8                          population 26.7029678
#9                             density 25.0846061
#10                       ep_otherrace 20.3243649

# True positives: 
# Regular classification tree
print("True Positive DTree:")
true_pos <- test[test$outbreak == TRUE & pred_dt == TRUE, ]
true_pos



# False negative: 
# Regular classification tree
print("False Negative DTree:")
false_negatives <- test[test$outbreak == TRUE & pred_dt == FALSE, ]
false_negatives



# False positives: 
# Regular classification tree
print("False Positive DTree:")
fp <- test[test$outbreak == FALSE & pred_dt == TRUE, ]
fp


# export FN results (merged_with_svi):
write_csv(false_negatives, "results/class_tree_FNresults.csv")

#saving key variables that the ctree model uses
write_csv(importance_df, "results/class_tree_important_vars.csv")
write_csv(res, "results/class_tree_best_combos.csv")
write_csv(results, "results/class_tree_paramtune_results.csv")
saveRDS(ctree_model, "results/class_tree_model.rds")







