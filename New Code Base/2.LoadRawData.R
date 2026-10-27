rm(list=ls())
source(here::here("New Code Base", "1.LoadLibraries.R"))

#### NEED TO ADD SOME OF THE EARLIER DATA PREPARATION STEPS,
#### E.G., HOW WAS merged_with_svi.csv CREATED ORIGINALLY?
# locate proj and load packages 
here::i_am("New Code Base/2.LoadRawData.R")

# unify county names
clean_county <- function(x) {
  x %>%
    str_to_lower() %>%
    str_remove(regex(",?\\s*texas$", ignore_case = TRUE)) %>%
    str_remove(regex("\\s+county$", ignore_case = TRUE)) %>%
    str_squish()
}

# import data
outbreak <- read.csv(here("data", "raw", "base", "count.csv")) %>%
  mutate(County = clean_county(County))
cve <- read.csv(here("data", "raw", "base", "CVE.csv")) %>%
  mutate(County = clean_county(County))
phr_conv <- read.csv(here("data", "raw", "brfss", "phr_conversion.csv"))%>%
  rename(phr = 'PHR') %>%
  mutate(County = clean_county(County))
df <- cve %>% inner_join(outbreak, by = "County", suffix = c("_cve", "_out"))

# create basic dataset(CVE, outbreak, enrollment, population, phr)
df2025 <- df %>%
transmute(
    County,
    cve = `X2025_cve`,
    outbreak = `X2025_out`
  )
df2025$cve <- gsub("%", "", df2025$cve)
df2025$cve <- as.numeric(df2025$cve)
enrollment <- read.csv(here("data", "raw", "base", "enrollment.csv"))
population <- read.csv(here("data", "raw", "base", "population.csv")) %>% 
  rename(population = `X2025_pop`) %>%
  mutate(County = clean_county(County))
enrollment <- enrollment %>%
  rename(enrollment = `Enrollment.Sum`) %>%
  mutate(County = clean_county(County)) %>%
  filter(County != "grand total")


# County level ACS data
census_key <- Sys.getenv("CENSUS_API_KEY")

if (!nzchar(census_key)) {
  stop("CENSUS_API_KEY was not found. Restart R after editing .Renviron.")
}

# 2024 ACS variables
acs_vars <- c(
  pct_hispanic     = "DP05_0090PE",
  pct_black        = "DP05_0097PE",
  pct_white        = "DP05_0096PE",
  pct_poverty      = "DP03_0128PE",
  pct_uninsured    = "DP03_0099PE",
  pct_college      = "DP02_0068PE",
  median_income    = "DP03_0062E",
  pct_foreign_born = "DP02_0094PE"
)

# Download and clean the data
texas_counties <- get_acs(
  geography = "county",
  state      = "TX",
  year       = 2024,
  survey     = "acs5",
  variables  = acs_vars,
  key        = census_key
) %>%
  transmute(
    County = clean_county(NAME),
    variable,
    estimate
  ) %>%
  pivot_wider(
    names_from  = variable,
    values_from = estimate
  ) %>%
  rename(
    pct_college      = DP02_0068P,
    pct_foreign_born = DP02_0094P,
    median_income    = DP03_0062,
    pct_uninsured    = DP03_0099P,
    pct_poverty      = DP03_0128P,
    pct_hispanic     = DP05_0090P,
    pct_white        = DP05_0096P,
    pct_black        = DP05_0097P
  )

svi <- read_csv(here("data", "raw", "svi.csv")) %>%
  mutate(County = clean_county(COUNTY)) %>%
  select(-ST, -STATE, -ST_ABBR, -STCNTY, -COUNTY, -FIPS, -LOCATION, -GeoLevel, -Comparison)


df2025 <- df2025 %>%
  left_join(enrollment, by = "County") %>%
  left_join(population, by = "County") %>%
  inner_join(phr_conv, by = "County") %>%
  left_join(texas_counties, by = "County") %>%
  left_join(svi, by = "County")%>%
  mutate(density = population / AREA_SQMI) %>%
  select(-AREA_SQMI)

write_csv(df2025, here("data", "datasets", "Clustering_Dataset.csv"))

# merge with BRFSS data
brfss <- read_csv(here("data", "raw", "brfss.csv"))

finaldf <- df2025 |>
  left_join(brfss, by = c("phr" = "PHR"))

write_csv(finaldf, here("data", "datasets", "FullData.csv"))



############## CLEANING merged_with_svi.csv
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

set.seed(100) # Set seed

# Load & Clean Column Names
# ############ UPDATE SO THAT YOU'RE NOT READING IN THE FILE AGAIN. SHOULD BE SAVED CLEANED. ############################
# datapath <- "c:/Users/martonosi/Dropbox/research/measles project/Summer 2026 Code Base/Predicting-Measles-Outbreaks-main/data/"
# #filename_mergedcounty <- paste(datapath, "merged_county_data.csv", sep="")
# filename_mergedcounty <- paste(datapath, "merged_with_svi.csv", sep="")
# merged <- read_csv(filename_mergedcounty, show_col_types = FALSE)|> clean_names()
merged <- janitor::clean_names(finaldf)
merged <- drop_redundant_yn_features(merged)

#SVI specific
merged <- merged %>% filter(!county %in% c("mcculloch", "mclennan", "mcmullen", "dewitt", "loving"))
merged <- merged %>% dplyr::select(-c("county", "phr"), -starts_with("m_"), -starts_with("mp_"), -starts_with("e_"), -starts_with("epl_"), -starts_with("spl_"), -starts_with("rpl_"), -starts_with("f_"))
merged <- merged[,colSums(is.na(merged)) == 0]
merged <- merged %>% dplyr::select(-c("ep_minrty", "ep_hisp", "ep_afam", "ep_pov150", "ep_uninsur"))


#################################################
# Create training and testing partition for regression
#################################################


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

# Train rows: 187 | Cols: 250 
# Test rows: 62 | Cols: 250 



###############################################
# CREATE REGRESSION FOLDS (NO SMOTE): 5 folds for regression
###############################################

# Create folds
set.seed(100)

# Recreate folds on transformed target
#strata <- ifelse(train$outbreak > 0, 1, 0)
folds <- createFolds(train$outbreak , k = 5, list = TRUE)



# # Export to csv
# write_csv(train, "intermed_regress_data/train_clean.csv")
# write_csv(test,  "intermed_regress_data/test_clean.csv")
# saveRDS(folds, file = "intermed_regress_data/folds.rds")
regress_dir <- here::here("New Code Base", "intermed_regress_data")
dir.create(regress_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(train, file.path(regress_dir, "train_clean.csv"))
write_csv(test, file.path(regress_dir, "test_clean.csv"))
saveRDS(folds, file.path(regress_dir, "folds.rds"))

########################################
# CREATE CLASSIFICATION DATA WITH SMOTE
###########################################

# DATA RELATED PARAMS
def_outbreak = 4 # number of cases to be considered as outbreak
perc_strata = 0.75 # proportion of data to be used for training

# SMOTE RELATED PARAMS
K_neighbors = 5 # number of nearest neighbors
Ratio_testtrainclass = 0 # for 50/50 outbreak/non-outbreak

# FIRST NEED BALANCED TRAIN/TEST DATA BASED ON CATEGORICAL OUTBREAK

# Classify as outbreak oR not
mergedclass <- merged
mergedclass$outbreak <- mergedclass$outbreak >= def_outbreak

# Create test/train split
set.seed(100) # Set seed

# Train/test split 
index <- createDataPartition(mergedclass$outbreak, p = 0.75, list = FALSE)
trainclass <- mergedclass[index, ]
testclass  <- mergedclass[-index, ]


# Ensure all columns are numeric 
non_num <- names(trainclass)[!sapply(trainclass, is.numeric)& names(testclass) != "outbreak"]
if (length(non_num) > 0) {
  trainclass[non_num] <- lapply(trainclass[non_num], as.numeric)
}

# Mirror same cleaning on test set 
testclass <- testclass[, names(testclass) %in% c(names(trainclass), "outbreak")]

non_num_test <- names(testclass)[!sapply(testclass, is.numeric) & names(testclass) != "outbreak"]
if (length(non_num_test) > 0) {
  testclass[non_num_test] <- lapply(testclass[non_num_test], as.numeric)
}

#test$outbreak <- as.numeric(test$outbreak)

stopifnot(sum(is.na(testclass)) == 0)

cat("Train rows:", nrow(trainclass), "| Cols:", ncol(trainclass), "\n")
cat("Test rows:",  nrow(testclass),  "| Cols:", ncol(testclass),  "\n")


# Train rows: 188 | Cols: 250                                                                                        
# Test rows: 61 | Cols: 250 

##################


set.seed(42)
# K for controlling the neighborhood, dup_size for ratio to achieve

smote_output <- SMOTE(
  X      = trainclass[, names(trainclass) != "outbreak"],
  target = trainclass$outbreak,
  K      = K_neighbors, 
  dup_size = Ratio_testtrainclass
)

trainclass_smote <- smote_output$data
# SMOTE put target data in "class" col and rename it
names(trainclass_smote)[names(trainclass_smote) == "class"] <- "outbreak"
trainclass_smote$outbreak <- as.logical(trainclass_smote$outbreak)
cat("\nClass balance after SMOTE:\n")
print(table(trainclass_smote$outbreak))

# FALSE  TRUE 
# 176   168 

head(trainclass_smote)




# Folds for cross validation;  10 folds for classification
set.seed(4)
folds_class <- createFolds(trainclass_smote$outbreak, k=10)

# # Export to csv
# write_csv(trainclass_smote, "intermed_class_data/trainclass_smote.csv")
# write_csv(testclass,  "intermed_class_data/testclass.csv")
# saveRDS(folds_class, file = "intermed_class_data/folds_class.rds")

class_dir <- here::here("New Code Base", "intermed_class_data")
dir.create(class_dir, recursive = TRUE, showWarnings = FALSE)

write_csv(trainclass_smote, file.path(class_dir, "trainclass_smote.csv"))
write_csv(testclass, file.path(class_dir, "testclass.csv"))
saveRDS(folds_class, file.path(class_dir, "folds_class.rds"))