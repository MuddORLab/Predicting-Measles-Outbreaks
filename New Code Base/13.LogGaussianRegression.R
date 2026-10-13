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


# 4b. Extract active variables at lambda.min
# Extract at lambda.min instead of lambda.1se
coefs_min <- coef(final_model)
selected_vars_min <- rownames(coefs_min)[coefs_min[, 1] != 0]
selected_vars_min <- setdiff(selected_vars_min, "(Intercept)")

cat("Variables selected at lambda.min:\n")
print(selected_vars_min)

# 5. Refit unpenalized linear model (lm) on log-transformed response
formula_lm <- as.formula(paste("y ~", paste(selected_vars_min, collapse = " + ")))
unpenalized_lm <- lm(formula_lm, data = dat_scaled)

# 6. Perform Stepwise Selection (AIC)
final_step_lm <- stepAIC(unpenalized_lm, direction = "both", trace = FALSE)
summary(final_step_lm)

# 7. Compute VIFs safely
vif_values <- car::vif(final_step_lm)
print(vif_values)

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
df_fore <- subset(df_fore, term != "(Intercept)")
label_map <- c("cve" = "Conscientious vaccine exemption (%)",
               "mammogram_past_2_yrs_40_ia_yes" = "Mammogram uptake (%, age 40+)",
               "poor_mental_health_14_days_14_or_more_days_pct_diff" = "Poor mental health (%)",
               "ep_nohsdp" = "No high-school diploma (%)",
               "enrollment_log" = "Log enrollments")
unmatched <- setdiff(df_fore$term, names(label_map))
if (length(unmatched)) message("No label for: ", paste(unmatched, collapse = ", "))
#df_fore$label <- recode(df_fore$term, label_map)
df_fore$label <- label_map[df_fore$term]
df_fore$direction <- ifelse(df_fore$estimate > 0, "Increases case counts", "Decreases case counts")
df_fore$stars <- symnum(df_fore$p.value, corr = FALSE, na = FALSE,
                        cutpoints = c(0, 0.001, 0.01, 0.05, 0.1, 1),
                        symbols = c("***", "**", "*", ".", "")) |> as.character()
df_fore$annot <- sprintf("beta == %.2f ~ 'p = %.4f %s'",
                         df_fore$estimate, df_fore$p.value, df_fore$stars)
df_fore$label <- factor(df_fore$label, levels = df_fore$label[order(df_fore$estimate)])
p <- ggplot(df_fore, aes(x = estimate, y = label, color = direction)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +
  geom_errorbar(aes(xmin = conf.low, xmax = conf.high),
                 height = 0.2, linewidth = 0.7) +
  geom_point(size = 3.5) +
  geom_text(aes(label = annot), vjust = -1.1, size = 5, show.legend = FALSE, parse = TRUE) +
  #scale_x_log10(expand = expansion(mult = c(0.05, 0.25))) +
  scale_y_discrete(labels = function(x) str_wrap(x, width = 14)) +
  scale_color_manual(values = c("Increases case counts" = "#D85A30",
                                "Decreases case counts" = "#378ADD")) +
  labs(x = NULL, y = NULL, color = NULL) +
  theme_minimal(base_size = 20) +
  theme(panel.grid.minor = element_blank(),
        legend.position = "top")+
  theme(panel.grid.minor = element_blank(),
        legend.position = "top",
        plot.caption  = element_text(size = 16, hjust = 0))

p

ggsave("results/regress_GLM_sigvars.pdf", p, width = 9, height = 6)
saveRDS(final_step_lm, "results/regress_GLM_model.rds")


train <-read_csv("intermed_regress_data/train_clean.csv")  # Restore train if needed later.
test <- read_csv("intermed_regress_data/test_clean.csv") # Restore test if needed later.