rm(list = ls())

test <-read_csv("intermed_class_data/testclass.csv")
train <-read_csv("intermed_class_data/trainclass_smote.csv")
folds <-readRDS("intermed_class_data/folds_class.rds")

y <- as.numeric(train$outbreak)
#y1 <- as.numeric(train$outbreak)
ytest <- as.numeric(test$outbreak)

train$enrollment_log <- log(train$enrollment+1)
test$enrollment_log <- log(test$enrollment+1)

X <- as.matrix(dplyr::select(train, -c("enrollment", "population","outbreak")))
testX <- as.matrix(dplyr::select(test, -c("enrollment", "population", "outbreak")))

#X1 <- as.matrix(dplyr::select(train, -c("enrollment", "population","outbreak")))
#testX1 <- as.matrix(dplyr::select(test, -c("enrollment", "population", "outbreak")))


# Start with full list of candidates
candidates <- colnames(X)
cols_to_drop <- c()

for(i in 1:(length(candidates)-1)) {
  for(j in (i + 1):length(candidates)) {
    
    # Skip if either column has already been flagged for removal
    if (i %in% cols_to_drop || j %in% cols_to_drop) next
    
    rho <- cor(X[, candidates[i]], X[, candidates[j]], 
               method = "spearman", use = "complete.obs")
    if (abs(rho) >= 0.7) {
      # Drop whichever has weaker correlation with outcome
      rho_i <- abs(cor(X[, candidates[i]], y, method = "spearman", use = "complete.obs"))
      rho_j <- abs(cor(X[, candidates[j]], y, method = "spearman", use = "complete.obs"))
      if (rho_i >= rho_j) {
        #candidates <- candidates[-j]  # Doesn't this reindex candidates?
        cols_to_drop <- c(cols_to_drop, j)
      } else {
        #candidates <- candidates[-i] 
        cols_to_drop <- c(cols_to_drop, i)
      }
    } 
    
  }
}

X <- X[, -unique(cols_to_drop), drop=FALSE]
testX <- testX[, -unique(cols_to_drop), drop=FALSE]
dim(X)
dim(testX)
#dim(X1)


# > dim(X)
# [1] 344  48
# > dim(testX)
# [1] 61 48



dat <- data.frame(y = y, X)
#dat1 <- data.frame(y1=y1, X1) # No removal of correlated variables

# CREATE MAPPING OF FOLDIDS

n_samples <- length(unlist(folds))

# 2. Initialize the vector
foldid <- integer(n_samples)

# 3. Assign the fold index to each corresponding row
for (i in seq_along(folds)) {
  foldid[folds[[i]]] <- i
}



### RESCALE DATA  ###
# 1. Scale continuous predictors to prevent extreme dot products
dat_scaled <- dat
continuous_cols <- setdiff(names(dat_scaled), "y")
dat_scaled[continuous_cols] <- scale(dat_scaled[continuous_cols])


# dat_scaled1 <- dat1
# continuous_cols1 <- setdiff(names(dat_scaled1), "y1")
# dat_scaled1[continuous_cols1] <- scale(dat_scaled1[continuous_cols1])

# 2. Create model matrix
x <- model.matrix(y ~ . - 1, data = dat_scaled)
#x1 <- model.matrix(y1 ~ . - 1, data = dat_scaled1)


### Elastic Net: Balance (alpha) between LASSO and Ridge Regression
### Useful when you have a large number of variables and many of them are
### highly correlated.  Tunes for both alpha (tradeoff between LASSO and Ridge)
### and Lambda (LASSO parameter).


# 3. Define grid of alpha values to search
alpha_grid <- seq(0, 1, by = 0.1)

# 4. Iterate over alpha grid and record results
results <- data.frame()

set.seed(100)
for (a in alpha_grid) {
  # Fit CV model for current alpha
  cv_fit <- cv.glmnet(
    x = x,
    y = y,
    family = "binomial",
    alpha = a,
    foldid = foldid,
    type.measure =  "deviance" # Or "auc"
  )

  # Identify min cross-validated deviance and associated lambda
  min_dev <- min(cv_fit$cvm)
  best_lam <- cv_fit$lambda.min

  # Store metrics
  results <- rbind(results, data.frame(
    alpha = a,
    lambda = best_lam,
    cv_error = min_dev
  ))
}

# 5. Extract optimal alpha and lambda
best_row <- results[which.min(results$cv_error), ]
print("Optimal Parameters:")
print(best_row)

# [1] "Optimal Parameters:"
# > print(best_row)
#     alpha      lambda  cv_error
# 11     1 0.004224175 0.4993514


# 6. Fit final model with optimal alpha and lambda
final_model <- glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = best_row$alpha,
  lambda = best_row$lambda
)

# View non-zero coefficients
coef(final_model)
sum(coef(final_model)[-1, ] != 0)

# > sum(coef(final_model)[-1, ] != 0)
# [1] 25

# CONFUSION MATRIX
preds <- predict(final_model, newx=testX, type= "class")


# 1. Force factor levels c(FALSE, TRUE)
preds_f   <- factor(preds, levels = c(0,1))

# 2. Generate 2x2 confusion matrix safely
logregtable <- table(Actual = ytest, Predicted = preds_f)
print(logregtable)

#         Predicted
# Actual  0  1
# 0       58  0
# 1       3  0


# Calculate Precision, Accuracy, and recall
TP <- logregtable[2, 2] # True Positives 
TN <- logregtable[1, 1] # True Negatives 
FP <- logregtable[1, 2] # False Positives 
FN <- logregtable[2, 1] # False Negatives 

accuracy    <- (TP + TN) / sum(logregtable) # total accuracy
recall <- TP / (TP + FN)                    # amount of positives guessed correctly out of all true positives
precision   <- TP / (TP + FP)               # amount of correct positive predictions


print(c(accuracy, recall, precision))

# [1] 0.9508197 0.0000000       NaN

# ## WORKED REALLY POORLY: NOW DO THE SAME WITH THE FULL MATRIX
# # 4. Iterate over alpha grid and record results
# results1 <- data.frame()
# 
# set.seed(100)
# for (a in alpha_grid) {
#   # Fit CV model for current alpha
#   cv_fit <- cv.glmnet(
#     x = x1, 
#     y = y1, 
#     family = "binomial", 
#     alpha = a, 
#     foldid = foldid,
#     type.measure =  "deviance" # Or "auc"
#   )
#   
#   # Identify min cross-validated deviance and associated lambda
#   min_dev <- min(cv_fit$cvm)
#   best_lam <- cv_fit$lambda.min
#   
#   # Store metrics
#   results1 <- rbind(results1, data.frame(
#     alpha = a,
#     lambda = best_lam,
#     cv_error = min_dev
#   ))
# }
# 
# # 5. Extract optimal alpha and lambda
# best_row1 <- results1[which.min(results1$cv_error), ]
# print("Optimal Parameters:")
# print(best_row1)
# 
# 
# # 6. Fit final model with optimal alpha and lambda
# final_model1 <- glmnet(
#   x = x1, 
#   y = y1, 
#   family = "binomial", 
#   alpha = best_row1$alpha, 
#   lambda = best_row1$lambda
# )
# 
# # View non-zero coefficients
# coef(final_model1)
# sum(coef(final_model1)[-1, ] != 0)
# 
# preds1 <- predict(final_model1, newx=testX1, type= "class")
# table(preds1, ytest)


############ OLD ##################
#train_LR <- glm(outbreak ~ ., data = train, family = binomial)
#train_LR  <- glm(y1~., data=dat_scaled1, family=binomial)
#train_LR_back <- step(train_LR,trace=0, direction = "backward")

# probs   <- predict(train_LR_back, newdata=test, type="response")
# summary(probs)   # check the range
# lr_pred <- ifelse(probs > 0.5, 1, 0)
# y_test  <- as.numeric(as.character(test$outbreak))
# table(Actual=as.factor(y_test), Predicted=as.factor(lr_pred))