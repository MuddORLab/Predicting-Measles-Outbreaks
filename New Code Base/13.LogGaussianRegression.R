rm(list=ls())
test <-read_csv("intermed_regress_data/test_clean.csv")
#train_balanced <-read_csv("intermed_regress_data/train_balanced.csv")  # SMOTE IS NOT USED.
train <-read_csv("intermed_regress_data/train_clean.csv")
folds <-readRDS("intermed_regress_data/folds.rds")

#PHR <- train$phr # REMOVE BECAUSE TRAIN DOESN'T HAVE THE PHR COLUMN
#offset <- log(train$enrollment+1)  # Not needed for log gaussian
train$enrollment_log <- log(train$enrollment+1)
outbreak_log <- log(train$outbreak+1)
#train <- train %>% dplyr::select(-c("phr", "enrollment", "population", "outbreak")) # WOULD BE NICE TO OMIT
train <- train %>% dplyr::select(-c("enrollment", "population", "outbreak")) # WOULD BE NICE TO OMIT

X <- as.matrix(train)
y <- outbreak_log

# Save fold info as vector
n_samples <- length(unlist(folds))

# 2. Initialize the vector
foldid <- integer(n_samples)

# 3. Assign the fold index to each corresponding row
for (i in seq_along(folds)) {
  foldid[folds[[i]]] <- i
}


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
dim(X)


dat <- data.frame(y = y, X)

### RESCALE DATA  ###
# 1. Scale continuous predictors to prevent extreme dot products
dat_scaled <- dat
continuous_cols <- setdiff(names(dat_scaled), "y")
dat_scaled[continuous_cols] <- scale(dat_scaled[continuous_cols])

# 2. Create model matrix
X_mat <- model.matrix(y ~ . - 1, data = dat_scaled)

# 3. Define grid of alpha values to search
alpha_grid <- seq(0, 1, by = 0.1)

# 4. Iterate over alpha grid and record results
results <- data.frame()

set.seed(100)
for (a in alpha_grid) {
  # Fit CV model for current alpha
  cv_fit <- cv.glmnet(
    x = X_mat, 
    y = dat_scaled$y, 
    family = "gaussian",
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
#     alpha   lambda  cv_error
# 1     0   1.272759 0.4976459



# 3. Fit Gaussian model using cross-validation
final_model <- glmnet(
  x = X_mat, 
  y = dat_scaled$y, 
  family = "gaussian", 
  alpha = best_row$alpha,
  lambda = best_row$lambda
)

# View non-zero coefficients
coef(final_model)
sum(coef(final_model)[-1, ] != 0)


# 4b. Extract active variables 
coefs_min <- coef(final_model)
selected_vars_min <- rownames(coefs_min)[coefs_min[, 1] != 0]
selected_vars_min <- setdiff(selected_vars_min, "(Intercept)")

cat("Variables selected:\n")
print(selected_vars_min)

# FIRST TEN PRINTED FOR REPRODUCIBILITY VERIFICATION:
# Variables selected:
#   [1] "cve"                                                            "pct_black"                                                     
# [3] "pct_poverty"                                                    "pct_uninsured"                                                 
# [5] "pct_college"                                                    "pct_foreign_born"                                              
# [7] "bmi_3_categories_recommended_range"                             "hlth_affected_activ_5_days_5_or_more_days"                     
# [9] "mammogram_past_2_yrs_40_ia_yes"                                 "pain_med_not_rx_no" 


# 5. Refit unpenalized linear model (lm) on log-transformed response
formula_lm <- as.formula(paste("y ~", paste(selected_vars_min, collapse = " + ")))
unpenalized_lm <- lm(formula_lm, data = dat_scaled)

# 6. Perform Stepwise Selection (AIC)
final_step_lm <- stepAIC(unpenalized_lm, direction = "both", trace = FALSE)
print(summary(final_step_lm))

# Call:
#   lm(formula = y ~ cve + hlth_affected_activ_5_days_5_or_more_days + 
#        pain_med_not_rx_no + reg_bp_check_yes + remove_teeth_1_to_5 + 
#        routine_checkup_within_the_past_2_years + sigm_past_5_yrs_50_75_ia_no + 
#        skin_cancer_no + ep_nohsdp + ep_age65 + ep_disabl + ep_sngpnt + 
#        ep_limeng + enrollment_log, data = dat_scaled)
# 
# Residuals:
#   Min      1Q  Median      3Q     Max 
# -1.2473 -0.2918 -0.0711  0.1841  2.9078 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.22999    0.04432   5.190 5.88e-07 ***
#   cve                                        0.22664    0.04949   4.579 8.93e-06 ***
#   hlth_affected_activ_5_days_5_or_more_days  0.38765    0.10259   3.779 0.000217 ***
#   pain_med_not_rx_no                        -0.14813    0.06181  -2.396 0.017630 *  
#   reg_bp_check_yes                          -0.44178    0.09343  -4.729 4.69e-06 ***
#   remove_teeth_1_to_5                       -0.44790    0.10374  -4.318 2.66e-05 ***
#   routine_checkup_within_the_past_2_years   -0.39098    0.09774  -4.000 9.38e-05 ***
#   sigm_past_5_yrs_50_75_ia_no                0.49215    0.11137   4.419 1.75e-05 ***
#   skin_cancer_no                             0.46084    0.09553   4.824 3.08e-06 ***
#   ep_nohsdp                                  0.36730    0.08202   4.478 1.37e-05 ***
#   ep_age65                                  -0.12270    0.07115  -1.724 0.086425 .  
# ep_disabl                                  0.12258    0.06057   2.024 0.044521 *  
#   ep_sngpnt                                 -0.11550    0.05707  -2.024 0.044544 *  
#   ep_limeng                                 -0.22576    0.08015  -2.817 0.005422 ** 
#   enrollment_log                             0.34476    0.07560   4.560 9.67e-06 ***
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.606 on 172 degrees of freedom
# Multiple R-squared:  0.4099,	Adjusted R-squared:  0.3618 
# F-statistic: 8.533 on 14 and 172 DF,  p-value: 7.49e-14

# 7. Compute VIFs safely
vif_values <- car::vif(final_step_lm)
print(vif_values)


#### VIFS
# cve hlth_affected_activ_5_days_5_or_more_days                        pain_med_not_rx_no 
# 1.240671                                  5.330684                                  1.935018 
# reg_bp_check_yes                       remove_teeth_1_to_5   routine_checkup_within_the_past_2_years 
# 4.420581                                  5.450337                                  4.837944 
# sigm_past_5_yrs_50_75_ia_no                            skin_cancer_no                                 ep_nohsdp 
# 6.281507                                  4.621805                                  3.407389 
# ep_age65                                 ep_disabl                                 ep_sngpnt 
# 2.564040                                  1.857847                                  1.649715 
# ep_limeng                            enrollment_log 
# 3.253732                                  2.894619 

# COMPUTE PREDICTION METRICS ON TRAINING SET
pred_tr   <- predict(final_step_lm, newdata = dat_scaled,   type = "response")

metrics_vec <- function(obs, pred, model, label) {
  #theta <- sigma(model)
  data.frame(
    set      = label,
    n        = length(obs),
    rmse     = sqrt(mean((obs - pred)^2)),
    mae      = mean(abs(obs - pred)),
    spearman = cor(pred, obs, method = "spearman"),
    auc      = as.numeric(pROC::auc(pROC::roc(as.integer(obs > 3), pred, quiet = TRUE)))
    #mean_ll  = mean(dnbinom(obs, size = theta, mu = pred, log = TRUE))
  )
}

print(metrics_vec(dat_scaled$y,   pred_tr,   final_step_lm, "train (in-sample)"))


#                set      n      rmse       mae  spearman      auc
#   1   train (in-sample) 187 0.5812034 0.3784004 0.4437151 0.989071




# 8. Residual plots
# Set the plot window to a 2x2 grid
# Plot all 4 diagnostic plots at once
png("results/regress_GLM_resids.png", width = 6, height = 4, units = "in", res = 300)
par(mfrow = c(2, 2))

plot(final_step_lm)
dev.off()

# Reset your layout back to default (1x1) when done
par(mfrow = c(1, 1))

# 9. Run prediction on test data
test$y      <- log(test$outbreak+1)
test$enrollment_log <- log(test$enrollment+1)
test_scaled <- test
continuous_cols_test <- setdiff(names(test_scaled), "y")

test_scaled[continuous_cols_test] <- scale(test_scaled[continuous_cols_test])


pred_test <- predict(final_step_lm, newdata = test_scaled, type = "response")


perf <- rbind(
  metrics_vec(dat_scaled$y,   pred_tr,   final_step_lm, "train (in-sample)"),
  metrics_vec(test_scaled$y, pred_test, final_step_lm, "test (out-of-sample)")
)
print(perf, row.names = FALSE, digits = 4)

# set                     n   rmse    mae spearman    auc
# train (in-sample)     187 0.5812 0.3784   0.4437 0.9891
# test (out-of-sample)    62 0.7577 0.4498   0.4119 0.7167


train_df   <- data.frame(pred = pred_tr,   obs = dat_scaled$y)
test_df <- data.frame(pred = pred_test, obs = test_scaled$y)
#outlier_thresh <- 3
#train_outliers <- train_df %>% filter(obs > outlier_thresh | pred > outlier_thresh)
#train_main     <- train_df %>% filter(obs <= outlier_thresh & pred <= outlier_thresh)

poster_theme <- theme_minimal(base_size = 15) +
  theme(
    plot.title    = element_text(size = 18, face = "bold"),
    plot.subtitle = element_text(size = 13),
    axis.title    = element_text(size = 16),
    axis.text     = element_text(size = 11),
    plot.caption  = element_text(size = 15, hjust = 0)
  )

p_train <- ggplot(train_df, aes(obs, pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_count(alpha = 0.6, color = "#F8766D") +
  scale_size_area(max_size = 6, guide = "none") +
  labs(
    y = "Predicted Measles Case Count (Log)",
    x = "Observed Measles Case Count (Log)",
    title = "Predicted vs. Observed Log Counts",
    subtitle = "Gaussian Linear Model (Training Set)"
    #caption = str_wrap(paste0(
    #  "An outlier county is omitted for readability (predicted = 4246.9, observed = 414)."
    #), 65)
  ) +
  poster_theme

p_test <- ggplot(test_df, aes(obs, pred)) +
  geom_abline(slope = 1, intercept = 0, linetype = "dashed", color = "grey40") +
  geom_count(alpha = 0.6, color = "#00BFC4") +
  scale_size_area(max_size = 6, guide = "none") +
  labs(
    y = "Predicted Measles Case Count (Log)",
    x = "Observed Measles Case Count (Log)",
    title = "Predicted vs. Observed Log Counts",
    subtitle = "Gaussian Linear Model (Testing Set)",
  ) +
  poster_theme
p_train
p_test


df_fore <- broom.mixed::tidy(final_step_lm, effects = "fixed", conf.int = TRUE)
print(df_fore)
df_fore_temp <- subset(df_fore, term != "(Intercept)")


# A tibble: 15 × 7
# term                                      estimate std.error statistic     p.value conf.low conf.high
# <chr>                                        <dbl>     <dbl>     <dbl>       <dbl>    <dbl>     <dbl>
#   1 (Intercept)                                  0.230    0.0443      5.19 0.000000588  0.143     0.317  
# 2 cve                                          0.227    0.0495      4.58 0.00000893   0.129     0.324  
# 3 hlth_affected_activ_5_days_5_or_more_days    0.388    0.103       3.78 0.000217     0.185     0.590  
# 4 pain_med_not_rx_no                          -0.148    0.0618     -2.40 0.0176      -0.270    -0.0261 
# 5 reg_bp_check_yes                            -0.442    0.0934     -4.73 0.00000469  -0.626    -0.257  
# 6 remove_teeth_1_to_5                         -0.448    0.104      -4.32 0.0000266   -0.653    -0.243  
# 7 routine_checkup_within_the_past_2_years     -0.391    0.0977     -4.00 0.0000938   -0.584    -0.198  
# 8 sigm_past_5_yrs_50_75_ia_no                  0.492    0.111       4.42 0.0000175    0.272     0.712  
# 9 skin_cancer_no                               0.461    0.0955      4.82 0.00000308   0.272     0.649  
# 10 ep_nohsdp                                    0.367    0.0820      4.48 0.0000137    0.205     0.529  
# 11 ep_age65                                    -0.123    0.0712     -1.72 0.0864      -0.263     0.0177 
# 12 ep_disabl                                    0.123    0.0606      2.02 0.0445       0.00304   0.242  
# 13 ep_sngpnt                                   -0.116    0.0571     -2.02 0.0445      -0.228    -0.00285
# 14 ep_limeng                                   -0.226    0.0802     -2.82 0.00542     -0.384    -0.0675 
# 15 enrollment_log                               0.345    0.0756      4.56 0.00000967   0.196     0.494   

# ############ NEED TO UPDATE THIS WITH CURRENT VARIABLES SELECTED BY MODEL ######################
# label_map <- c("cve" = "Conscientious vaccine exemption (%)",
#                "mammogram_past_2_yrs_40_ia_yes" = "Mammogram uptake (%, age 40+)",
#                "poor_mental_health_14_days_14_or_more_days_pct_diff" = "Poor mental health (%)",
#                "ep_nohsdp" = "No high-school diploma (%)",
#                "enrollment_log" = "Log enrollments")
# unmatched <- setdiff(df_fore_temp$term, names(label_map))
# if (length(unmatched)) message("No label for: ", paste(unmatched, collapse = ", "))
# #df_fore_temp$label <- recode(df_fore_temp$term, label_map)
# df_fore_temp$label <- label_map[df_fore_temp$term]
# df_fore_temp$direction <- ifelse(df_fore_temp$estimate > 0, "Increases case counts", "Decreases case counts")
# df_fore_temp$stars <- symnum(df_fore_temp$p.value, corr = FALSE, na = FALSE,
#                         cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
#                         symbols = c("***", "**", "*", ".", "")) |> as.character()
# df_fore_temp$annot <- sprintf("beta == %.2f ~ 'p = %.4f %s'",
#                          df_fore_temp$estimate, df_fore_temp$p.value, df_fore_temp$stars)
# df_fore_temp$label <- factor(df_fore_temp$label, levels = df_fore_temp$label[order(df_fore_temp$estimate)])
# p <- ggplot(df_fore_temp, aes(x = estimate, y = label, color = direction)) +
#   geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
#   geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
#                  height = 0.2, linewidth = 0.7) +
#   geom_point(size = 3.5) +
#   geom_text(aes(label = annot), vjust = -1.1, size = 5, show.legend = FALSE, parse = TRUE) +
#   #scale_x_log10(expand = expansion(mult = c(0.05, 0.25))) +
#   scale_y_discrete(labels = function(x) str_wrap(x, width = 14)) +
#   scale_color_manual(values = c("Increases case counts" = "#D85A30",
#                                 "Decreases case counts" = "#378ADD")) +
#   labs(x = NULL, y = NULL, color = NULL) +
#   theme_minimal(base_size = 20) +
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "top")+
#   theme(panel.grid.minor = element_blank(),
#         legend.position = "top",
#         plot.caption  = element_text(size = 16, hjust = 0))
# 
# p

#ggsave("results/regress_GLM_sigvars.pdf", p, width = 9, height = 6)
saveRDS(final_step_lm, "results/regress_GLM_model.rds")
write_csv(df_fore, "results/regress_GLM_estimates.csv")
