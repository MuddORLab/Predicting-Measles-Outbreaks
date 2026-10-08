# Create training and testing partitioning for classification methods
# Reads in BRFSS+SVI+census+other PHR-level data for models
# Saves training and testing files for use by all classification methods
# Saves folds for use by all classification methods for 5-fold CV
# DOES NOT RUN SMOTE.

rm(list=ls())


# Read in BRFSS+SVI+census+other PHR-level data
### NEED TO UPDATE THIS SO THAT ALL CLEANING HAPPENS IN FILE 2.
datapath <- "c:/Users/martonosi/Dropbox/research/measles project/Summer 2026 Code Base/Predicting-Measles-Outbreaks-main/data/"
#filename_mergedcounty <- paste(datapath, "merged_county_data.csv", sep="")
filename_mergedcounty <- paste(datapath, "merged_with_svi.csv", sep="")
merged <- read_csv(filename_mergedcounty, show_col_types = FALSE)|> clean_names()

# Data Cleaning
drop_redundant_yn_features <- function(df) {
  # Get all column names
  cols <- names(df)
  
  # Find Yes/No pairs by stripping the suffix
  yes_cols <- cols[grepl("_yes", cols)]
  no_cols  <- cols[grepl("_no",  cols)]
  
  # Get base names for each
  yes_bases <- sub("_diff$", "", sub("_yes", "", yes_cols))
  no_bases  <- sub("_diff$", "", sub("_no",  "", no_cols))
  
  # Find bases that have BOTH a Yes and No column
  paired_bases <- intersect(yes_bases, no_bases)
  
  cols_to_drop <- c()
  
  for (base in paired_bases) {
    yes_col <- yes_cols[yes_bases == base]
    no_col  <- no_cols[no_bases == base]
    
    # Count NAs in each
    yes_nas <- sum(is.na(df[[yes_col]]))
    no_nas  <- sum(is.na(df[[no_col]]))
    
    # Drop whichever has more NAs 
    # If tied drop "No" (keep "Yes")
    if (no_nas >= yes_nas) {
      cols_to_drop <- c(cols_to_drop, no_col)
    } else {
      cols_to_drop <- c(cols_to_drop, yes_col)
    }
  }
  
  cat("Dropping", length(cols_to_drop), "redundant columns:\n")
  #cat(paste(" ", cols_to_drop), sep = "\n")
  
  df[, !names(df) %in% cols_to_drop]
}


merged <- drop_redundant_yn_features(merged)

#SVI specific
merged <- merged %>% filter(!county %in% c("Mcculloch", "Mclennan", "Mcmullen", "Dewitt", "Loving"))
merged$density <- merged$population*1.0/merged$area_sqmi
merged <- merged %>% dplyr::select(-c("county", "phr", "area_sqmi"), -starts_with("m_"), -starts_with("mp_"), -starts_with("e_"), -starts_with("epl_"), -starts_with("spl_"), -starts_with("rpl_"), -starts_with("f_"))
merged <- merged[,colSums(is.na(merged)) == 0]
merged <- merged %>% dplyr::select(-c("ep_minrty", "ep_hisp", "ep_afam", "ep_pov150", "ep_uninsur"))





# Create test/train split
set.seed(100) # Set seed

# Train/test split 
index <- createDataPartition(merged$outbreak, p = 0.75, list = FALSE)
train <- merged[index, ]
test  <- merged[-index, ]


# Ensure all columns are numeric 
non_num <- names(train)[!sapply(train, is.numeric)]
if (length(non_num) > 0) {
  train[non_num] <- lapply(train[non_num], as.numeric)
}

# Mirror same cleaning on test set 
test <- test[, names(test) %in% c(names(train), "outbreak")]

non_num_test <- names(test)[!sapply(test, is.numeric) & names(test) != "outbreak"]
if (length(non_num_test) > 0) {
  test[non_num_test] <- lapply(test[non_num_test], as.numeric)
}

test$outbreak <- as.numeric(test$outbreak)

stopifnot(sum(is.na(test)) == 0)

cat("Train rows:", nrow(train), "| Cols:", ncol(train), "\n")
cat("Test rows:",  nrow(test),  "| Cols:", ncol(test),  "\n")



# 
# # Simple Native R SMOTER function
# smoter_r <- function(data, target_col, k = 5, over_ratio = 1, threshold = 2) {
#   # Identify rare cases (above threshold) vs normal cases
#   rare_idx <- which(data[[target_col]] >= threshold)
#   if (length(rare_idx) < k + 1) {
#     stop("Not enough rare samples to find k nearest neighbors.")
#   }
#   
#   rare_data <- data[rare_idx, ]
#   numeric_cols <- setdiff(names(data)[sapply(data, is.numeric)], target_col)
#   
#   synthetic_rows <- list()
#   
#   
#   # Generate synthetic samples for rare cases
#   for (i in seq_len(nrow(rare_data))) {
#     # Compute Euclidean distance to all other rare samples
#     dists <- sqrt(rowSums((as.matrix(rare_data[, numeric_cols]) - 
#                              matrix(as.numeric(rare_data[i, numeric_cols]), 
#                                     nrow = nrow(rare_data), 
#                                     ncol = length(numeric_cols), 
#                                     byrow = TRUE))^2))
#     
#     # Get k nearest neighbors (excluding self)
#     nn_idx <- order(dists)[2:(k + 1)]
#     
#     for (rep in seq_len(over_ratio)) {
#       neighbor_idx <- sample(nn_idx, 1)
#       diff <- rare_data[neighbor_idx, numeric_cols] - rare_data[i, numeric_cols]
#       gap <- runif(1)
#       
#       # Interpolate numeric features and target variable
#       new_row <- rare_data[i, ]
#       new_row[, numeric_cols] <- rare_data[i, numeric_cols] + gap * diff
#       #new_row[[target_col]] <- rare_data[i, target_col] + gap * (rare_data[neighbor_idx, target_col] - rare_data[i, target_col])
#       new_row[target_col] <- rare_data[i, target_col] + gap * (rare_data[neighbor_idx, target_col] - rare_data[i, target_col])
#       
#       synthetic_rows[[length(synthetic_rows) + 1]] <- new_row
#     }
#   }
#   
#   synthetic_df <- bind_rows(synthetic_rows)
#   bind_rows(data, synthetic_df)
# }

# # --- Usage ---
# 
# set.seed(100)
# 
# # Balance dataset using rare threshold = 2 (matches control point)
# train_balanced <- smoter_r(
#   data = train,
#   target_col = "outbreak",
#   k = 5,
#   over_ratio = 1,
#   threshold = 2
# )
# 
# # Output binned distribution
# binned_outbreak <- cut(
#   train_balanced$outbreak,
#   breaks = c(-0.1, 0, 1, 2, 5, 10, Inf),
#   right = TRUE
# )
# 
# print(table(binned_outbreak))
# 


# For log transformations
set.seed(100)

# Recreate folds on transformed target
#strata <- ifelse(train$outbreak > 0, 1, 0)
folds <- createFolds(train$outbreak , k = 5, list = TRUE)



# Export to csv
write_csv(train, "intermed_regress_data/train_clean.csv")
write_csv(test,  "intermed_regress_data/test_clean.csv")
#write_csv(train_balanced, "intermed_regress_data/train_balanced.csv")
saveRDS(folds, file = "intermed_regress_data/folds.rds")


