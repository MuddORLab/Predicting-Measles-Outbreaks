# SET WORKING DIRECTORY TO THE DIRECTORY WHERE THIS FILE IS STORED.
rm(list=ls())
source("1.LoadLibraries.R")



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
#[1] 344  44
dim(testX)
#[1] 61 44
#dim(X1)



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

# Warning message:
#from glmnet C++ code (error code -94); Convergence for 94th lambda value not reached after maxit=100000 iterations; solutions for larger lambdas returned 


# 5. Extract optimal alpha and lambda
best_row <- results[which.min(results$cv_error), ]
print("Optimal Parameters:")
print(best_row)

# [1] "Optimal Parameters:"
#> print(best_row)
#   alpha      lambda  cv_error
#2   0.1 0.002934465 0.4845936

class_GLM_optparam <- best_row


# 6. Fit model with optimal alpha and lambda
final_model <- glmnet(
  x = x,
  y = y,
  family = "binomial",
  alpha = best_row$alpha,
  lambda = best_row$lambda
)

# View non-zero coefficients
coef(final_model)
print(sum(coef(final_model)[-1, ] != 0))


# > sum(coef(final_model)[-1, ] != 0)
# [1] 42


# 4b. Extract active variables 
coefs_min <- coef(final_model)
selected_vars_min <- rownames(coefs_min)[coefs_min[, 1] != 0]
selected_vars_min <- setdiff(selected_vars_min, "(Intercept)")

cat("Variables selected:\n")
print(selected_vars_min)

#[1] "cve"                                                           "pct_college"                                                  
#[3] "pct_foreign_born"                                              "median_income"                                                
#[5] "pct_uninsured"                                                 "pct_white"                                                    
#[7] "pct_black"                                                     "ep_unemp"                                                     
#[9] "ep_hburd"                                                      "ep_age17"                                                     
#[11] "ep_disabl"                                                     "ep_sngpnt"                                                    
#[13] "ep_mobile"                                                     "ep_crowd"                                                     
#[15] "ep_noveh"                                                      "ep_groupq"                                                    
#[17] "ep_noint"                                                      "ep_asian"                                                     
#[19] "ep_aian"                                                       "ep_nhpi"                                                      
#[21] "ep_twomore"                                                    "ep_otherrace"                                                 
#[23] "hlth_affected_activ_5_days_5_or_more_days"                     "last_smoked_10_years"                                         
#[25] "routine_checkup_in_the_past_yes"                               "suicide_attempt_no"                                           
#[27] "ever_had_blood_stool_test_yes_pct_diff"                        "ever_had_hiv_test_yes_pct_diff"                               
#[29] "flu_shot_in_past_year_yes_pct_diff"                            "frequency_of_smoking_every_day_pct_diff"                      
#[31] "frequency_of_smoking_not_at_all_pct_diff"                      "hlth_affected_activ_14_days_14_or_more_days_pct_diff"         
#[33] "last_dentist_visit_5_or_more_years_pct_diff"                   "last_dentist_visit_within_the_past_2_years_pct_diff"          
#[35] "mammogram_yes_pct_diff"                                        "obese_yes_pct_diff"                                           
#[37] "overweight_or_obese_yes_pct_diff"                              "pneumonia_shot_18_yes_pct_diff"                               
#[39] "poor_mental_health_14_days_none_to_less_than_14_days_pct_diff" "skin_cancer_no_pct_diff"                                      
#[41] "smokeless_tobacco_use_not_at_all_pct_diff"                     "enrollment_log"                                                  


print(final_model)
# Call:  glmnet(x = x, y = y, family = "binomial", alpha = best_row$alpha,      lambda = best_row$lambda) 
#
#Df  %Dev   Lambda
#1 42 75.77 0.002934

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

df_fore <- broom.mixed::tidy(final_model, effects = "fixed", conf.int = TRUE)
print(df_fore, n=sum(coef(final_model) != 0))

# term                                                           step estimate  lambda dev.ratio
# <chr>                                                         <dbl>    <dbl>   <dbl>     <dbl>
#   1 (Intercept)                                                       1  -1.71   0.00293     0.758
# 2 cve                                                               1   1.00   0.00293     0.758
# 3 pct_college                                                       1  -1.03   0.00293     0.758
# 4 pct_foreign_born                                                  1   0.0388 0.00293     0.758
# 5 median_income                                                     1  -0.225  0.00293     0.758
# 6 pct_uninsured                                                     1   0.0266 0.00293     0.758
# 7 pct_white                                                         1  -0.261  0.00293     0.758
# 8 pct_black                                                         1  -0.915  0.00293     0.758
# 9 ep_unemp                                                          1  -0.439  0.00293     0.758
# 10 ep_hburd                                                          1   0.811  0.00293     0.758
# 11 ep_age17                                                          1   1.20   0.00293     0.758
# 12 ep_disabl                                                         1   1.02   0.00293     0.758
# 13 ep_sngpnt                                                         1  -0.316  0.00293     0.758
# 14 ep_mobile                                                         1  -0.638  0.00293     0.758
# 15 ep_crowd                                                          1  -0.333  0.00293     0.758
# 16 ep_noveh                                                          1  -0.363  0.00293     0.758
# 17 ep_groupq                                                         1   0.361  0.00293     0.758
# 18 ep_noint                                                          1   0.336  0.00293     0.758
# 19 ep_asian                                                          1  -2.04   0.00293     0.758
# 20 ep_aian                                                           1   0.362  0.00293     0.758
# 21 ep_nhpi                                                           1  -0.433  0.00293     0.758
# 22 ep_twomore                                                        1   0.600  0.00293     0.758
# 23 ep_otherrace                                                      1  -0.697  0.00293     0.758
# 24 hlth_affected_activ_5_days_5_or_more_days                         1  -0.710  0.00293     0.758
# 25 last_smoked_10_years                                              1   0.769  0.00293     0.758
# 26 routine_checkup_in_the_past_yes                                   1   0.0163 0.00293     0.758
# 27 suicide_attempt_no                                                1  -0.509  0.00293     0.758
# 28 ever_had_blood_stool_test_yes_pct_diff                            1  -0.0401 0.00293     0.758
# 29 ever_had_hiv_test_yes_pct_diff                                    1  -0.392  0.00293     0.758
# 30 flu_shot_in_past_year_yes_pct_diff                                1  -0.0727 0.00293     0.758
# 31 frequency_of_smoking_every_day_pct_diff                           1  -0.288  0.00293     0.758
# 32 frequency_of_smoking_not_at_all_pct_diff                          1   0.187  0.00293     0.758
# 33 hlth_affected_activ_14_days_14_or_more_days_pct_diff              1  -0.149  0.00293     0.758
# 34 last_dentist_visit_5_or_more_years_pct_diff                       1  -0.472  0.00293     0.758
# 35 last_dentist_visit_within_the_past_2_years_pct_diff               1   0.353  0.00293     0.758
# 36 mammogram_yes_pct_diff                                            1  -0.561  0.00293     0.758
# 37 obese_yes_pct_diff                                                1  -0.103  0.00293     0.758
# 38 overweight_or_obese_yes_pct_diff                                  1   0.770  0.00293     0.758
# 39 pneumonia_shot_18_yes_pct_diff                                    1   0.600  0.00293     0.758
# 40 poor_mental_health_14_days_none_to_less_than_14_days_pct_diff     1   0.766  0.00293     0.758
# 41 skin_cancer_no_pct_diff                                           1   0.562  0.00293     0.758
# 42 smokeless_tobacco_use_not_at_all_pct_diff                         1  -0.491  0.00293     0.758
# 43 enrollment_log                                                    1   2.91   0.00293     0.758

saveRDS(final_model, "results/class_GLM_model.rds")
write_csv(df_fore, "results/class_GLM_estimates.csv")
write_csv(class_GLM_optparam, "results/class_GLM_optparam.csv")


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