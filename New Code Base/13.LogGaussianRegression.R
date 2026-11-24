# SET WORKING DIRECTORY TO THE DIRECTORY WHERE THIS FILE IS STORED.
rm(list=ls())
source("1.LoadLibraries.R")


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
#[1] "cve"                                                           
#[2] "pct_college"                                                   
##[3] "pct_foreign_born"                                              
#[4] "median_income"                                                 
#[5] "pct_uninsured"                                                 
#[6] "pct_poverty"                                                   
#[7] "pct_black"                                                     
#[8] "ep_unemp"                                                      
#[9] "ep_hburd"                                                      
#[10] "ep_nohsdp"        

# 5. Refit unpenalized linear model (lm) on log-transformed response
formula_lm <- as.formula(paste("y ~", paste(selected_vars_min, collapse = " + ")))
unpenalized_lm <- lm(formula_lm, data = dat_scaled)

# 6. Perform Stepwise Selection (AIC)
final_step_lm <- stepAIC(unpenalized_lm, direction = "both", trace = FALSE)
print(summary(final_step_lm))

# Call:
#   lm(formula = y ~ cve + pct_college + pct_poverty + ep_hburd + 
#        ep_nohsdp + ep_age65 + ep_sngpnt + ep_groupq + hlth_affected_activ_5_days_5_or_more_days + 
#        pain_med_not_rx_no + reg_bp_check_yes + removeteeth_1_to_5 + 
#        routine_checkup_within_the_past_2_years + sigm_past_5_yrs_50_75_ia_no + 
#        skin_cancer_no + enrollment_log, data = dat_scaled)
# 
# Residuals:
#   Min       1Q   Median       3Q      Max 
# -0.97679 -0.31589 -0.05171  0.20485  2.96065 
# 
# Coefficients:
#   Estimate Std. Error t value Pr(>|t|)    
# (Intercept)                                0.22999    0.04476   5.139 7.53e-07 ***
#   cve                                        0.25373    0.05077   4.997 1.43e-06 ***
#   pct_college                               -0.16694    0.06825  -2.446 0.015454 *  
#   pct_poverty                               -0.09026    0.06158  -1.466 0.144580    
# ep_hburd                                   0.11948    0.06252   1.911 0.057685 .  
# ep_nohsdp                                  0.19738    0.07540   2.618 0.009649 ** 
#   ep_age65                                  -0.11117    0.07234  -1.537 0.126231    
# ep_sngpnt                                 -0.16924    0.05900  -2.868 0.004649 ** 
#   ep_groupq                                 -0.08110    0.05057  -1.604 0.110661    
# hlth_affected_activ_5_days_5_or_more_days  0.38532    0.10344   3.725 0.000265 ***
#   pain_med_not_rx_no                        -0.13142    0.06198  -2.120 0.035433 *  
#   reg_bp_check_yes                          -0.38572    0.09299  -4.148 5.29e-05 ***
#   removeteeth_1_to_5                        -0.39414    0.10346  -3.810 0.000194 ***
#   routine_checkup_within_the_past_2_years   -0.31607    0.09626  -3.284 0.001244 ** 
#   sigm_past_5_yrs_50_75_ia_no                0.43864    0.11227   3.907 0.000135 ***
#   skin_cancer_no                             0.40866    0.09494   4.305 2.82e-05 ***
#   enrollment_log                             0.23500    0.08505   2.763 0.006357 ** 
#   ---
#   Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1
# 
# Residual standard error: 0.612 on 170 degrees of freedom
# Multiple R-squared:  0.4051,	Adjusted R-squared:  0.3491 
# F-statistic: 7.235 on 16 and 170 DF,  p-value: 1.194e-12

# 7. Compute VIFs safely
vif_values <- car::vif(final_step_lm)
print(vif_values)


#### VIFS
# cve                               pct_college                               pct_poverty 
# 1.280088                                  2.312659                                  1.883256 
# ep_hburd                                 ep_nohsdp                                  ep_age65 
# 1.940927                                  2.822868                                  2.598660 
# ep_sngpnt                                 ep_groupq hlth_affected_activ_5_days_5_or_more_days 
# 1.728589                                  1.269977                                  5.312556 
# pain_med_not_rx_no                          reg_bp_check_yes                        removeteeth_1_to_5 
# 1.907734                                  4.293770                                  5.314730 
# routine_checkup_within_the_past_2_years               sigm_past_5_yrs_50_75_ia_no                            skin_cancer_no 
# 4.600674                                  6.258750                                  4.475507 
# enrollment_log 
# 3.591699 

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


# set   n      rmse       mae  spearman      auc
# 1 train (in-sample) 187 0.5835496 0.3865532 0.4334138 0.989071



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

# set   n   rmse    mae spearman    auc
# train (in-sample) 187 0.5835 0.3866   0.4334 0.9891
# test (out-of-sample)  62 0.7551 0.4373   0.4519 0.7000


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

#    term                                      estimate std.error statistic     p.value conf.low conf.high

# 1 (Intercept)                                 0.230     0.0448      5.14 0.000000753  0.142     0.318  
# 2 cve                                         0.254     0.0508      5.00 0.00000143   0.154     0.354  
# 3 pct_college                                -0.167     0.0682     -2.45 0.0155      -0.302    -0.0322 
# 4 pct_poverty                                -0.0903    0.0616     -1.47 0.145       -0.212     0.0313 
# 5 ep_hburd                                    0.119     0.0625      1.91 0.0577      -0.00394   0.243  
# 6 ep_nohsdp                                   0.197     0.0754      2.62 0.00965      0.0485    0.346  
# 7 ep_age65                                   -0.111     0.0723     -1.54 0.126       -0.254     0.0316 
# 8 ep_sngpnt                                  -0.169     0.0590     -2.87 0.00465     -0.286    -0.0528 
# 9 ep_groupq                                  -0.0811    0.0506     -1.60 0.111       -0.181     0.0187 
# 10 hlth_affected_activ_5_days_5_or_more_days   0.385     0.103       3.73 0.000265     0.181     0.590  
# 11 pain_med_not_rx_no                         -0.131     0.0620     -2.12 0.0354      -0.254    -0.00907
# 12 reg_bp_check_yes                           -0.386     0.0930     -4.15 0.0000529   -0.569    -0.202  
# 13 removeteeth_1_to_5                         -0.394     0.103      -3.81 0.000194    -0.598    -0.190  
# 14 routine_checkup_within_the_past_2_years    -0.316     0.0963     -3.28 0.00124     -0.506    -0.126  
# 15 sigm_past_5_yrs_50_75_ia_no                 0.439     0.112       3.91 0.000135     0.217     0.660  
# 16 skin_cancer_no                              0.409     0.0949      4.30 0.0000282    0.221     0.596  
# 17 enrollment_log                              0.235     0.0850      2.76 0.00636      0.0671    0.403 

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
