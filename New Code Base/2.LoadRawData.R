rm(list=ls())


############## CREATING merged_with_svi.csv
# Ported from Methods/Data Processing/3CreatingDatasets.ipynb

datapath <- "../data/"
rawpath  <- paste0(datapath, "raw/")


#################################################
# Base county data
#################################################

# Load raw files
outbreak   <- read.csv(paste0(rawpath, "base/count.csv"))
cve        <- read.csv(paste0(rawpath, "base/CVE.csv"))
enrollment <- read.csv(paste0(rawpath, "base/enrollment.csv"))
population <- read.csv(paste0(rawpath, "base/population.csv"))
phr_conv   <- read.csv(paste0(rawpath, "brfss/phr_conversion.csv"))

# "ANDERSON COUNTY" -> "Anderson"
enrollment <- enrollment %>%
  mutate(County = str_to_lower(County),
         County = str_remove(County, " county"),
         County = str_to_title(str_trim(County))) %>%
  filter(County != "Grand Total")

# Keep 2025 CVE and outbreak counts
county_data <- cve %>%
  inner_join(outbreak, by = "County", suffix = c("_cve", "_out")) %>%
  transmute(County,
            cve      = as.numeric(gsub("%", "", X2025_cve)),
            outbreak = X2025_out)

# Add enrollment, population and PHR
county_data <- county_data %>%
  left_join(enrollment, by = "County") %>%
  left_join(population, by = "County") %>%
  inner_join(phr_conv, by = "County") %>%
  transmute(County     = str_to_title(County),
            cve,
            outbreak,
            enrollment = Enrollment.Sum,
            population = X2025_pop,
            PHR)


#################################################
# Census data (ACS 5-year, 2024)
#################################################

acs_vars <- c(
  pct_hispanic     = "DP05_0090PE",
  pct_black        = "DP05_0045PE",
  pct_white        = "DP05_0037PE",
  pct_poverty      = "DP03_0119PE",
  pct_uninsured    = "DP03_0099PE",
  pct_college      = "DP02_0068PE",
  median_income    = "DP03_0062PE",
  pct_foreign_born = "DP02_0093PE"
)

# Pull from the Census API if CENSUS_API_KEY is set, else use saved file
if (Sys.getenv("CENSUS_API_KEY") != "") {
  texas_census <- get_acs(geography = "county", state = "TX",
                          year = 2024, survey = "acs5",
                          variables = acs_vars, output = "wide")

  # "Anderson County, Texas" -> "Anderson", keep estimates only
  texas_census <- texas_census %>%
    mutate(County = str_to_lower(NAME),
           County = str_remove(County, " county, texas"),
           County = str_to_title(str_trim(County))) %>%
    dplyr::select(County, all_of(names(acs_vars)))
} else {
  texas_census <- read_csv(paste0(datapath, "texas_census.csv"),
                           show_col_types = FALSE)
}


#################################################
# BRFSS data by PHR
#################################################

scraped_phrs <- c(1, 8, 11)               # 2014 values come from the scraped file
pct_cols     <- LETTERS[seq(4, 24, by = 2)]  # % columns D, F, ..., X

# Response labels (row 8) and Total row (row 10) of every sheet
read_brfss <- function(year, phr) {
  path <- paste0(rawpath, "brfss/", year, "_PHR", phr,
                 "_BRFSS_Summary_Tables.xlsx")
  wb <- wb_load(path, data_only = TRUE)
  sheets <- setdiff(wb_get_sheet_names(wb), "Index")

  bind_rows(lapply(sheets, function(s) {
    x <- wb_to_df(wb, sheet = s, col_names = FALSE,
                  skip_empty_rows = FALSE, skip_empty_cols = FALSE)
    x[, setdiff(pct_cols, names(x))] <- NA
    labels <- trimws(as.character(x["8", pct_cols]))
    pcts   <- suppressWarnings(as.numeric(x["10", pct_cols]))
    keep   <- !labels %in% c(NA, "NA", "", "None", "0")
    tibble(sheet = s, response = labels[keep], phr = phr, pct = pcts[keep])
  }))
}

# Load 2024 and 2014 workbooks
records_2024 <- bind_rows(lapply(1:11, function(p) read_brfss(2024, p)))
records_2014 <- bind_rows(lapply(setdiff(1:11, scraped_phrs),
                                 function(p) read_brfss(2014, p)))

# Load scraped 2014 values
scraped_2014 <- read_csv(paste0(datapath, "created/brfss_scraped_2014.csv"),
                         show_col_types = FALSE) %>%
  filter(phr %in% scraped_phrs, !is.na(percent))

# 2024 -> 2014 sheet/response names (built by name matching in 3CreatingDatasets.ipynb)
name_map <- read_csv(paste0(datapath, "created/brfss_2014_2024_map.csv"),
                     show_col_types = FALSE)


#################################################
# BRFSS values and 2014 -> 2024 % difference
#################################################

# 2014 Excel values under their 2024 names
excel_2014 <- name_map %>%
  inner_join(records_2014, by = c("sheet_2014" = "sheet",
                                  "response_2014" = "response"),
             relationship = "many-to-many") %>%
  dplyr::select(sheet = sheet_2024, response = response_2024,
                phr, v14_excel = pct)

# Scraped 2014 values are named like "a_one_c_no"
scraped_2014 <- scraped_2014 %>%
  dplyr::select(scraped_var = variable, phr, v14_scraped = percent)

# One row per variable and PHR, with 2024 and 2014 values
brfss <- records_2024 %>%
  distinct(sheet, response) %>%
  crossing(phr = 1:11) %>%
  mutate(variable    = paste(sheet, "-", response),
         scraped_var = str_to_lower(str_replace_all(variable, "[^A-Za-z0-9]+", "_")),
         scraped_var = str_remove_all(scraped_var, "^_+|_+$")) %>%
  left_join(records_2024 %>% rename(v24 = pct),
            by = c("sheet", "response", "phr")) %>%
  left_join(excel_2014, by = c("sheet", "response", "phr")) %>%
  left_join(scraped_2014, by = c("scraped_var", "phr"))

# Percent difference from 2014 to 2024
brfss <- brfss %>%
  mutate(v14      = ifelse(phr %in% scraped_phrs, v14_scraped, v14_excel),
         pct_diff = round((v24 - v14) / abs(v14) * 100, 4),
         pct_diff = ifelse(v14 == 0, NA, pct_diff)) %>%
  arrange(variable)

# Wide format: one row per PHR
brfss_values <- brfss %>%
  dplyr::select(PHR = phr, variable, v24) %>%
  pivot_wider(names_from = variable, values_from = v24)
brfss_diffs <- brfss %>%
  dplyr::select(PHR = phr, variable, pct_diff) %>%
  pivot_wider(names_from = variable, values_from = pct_diff,
              names_glue = "{variable}_pct_diff")
brfss_phr <- left_join(brfss_values, brfss_diffs, by = "PHR")


#################################################
# SVI and final merge
#################################################

# Load SVI, drop ID columns
svi <- read_csv(paste0(datapath, "svi_interactive_map.csv"),
                show_col_types = FALSE) %>%
  mutate(County = gsub(" County", "", COUNTY)) %>%
  dplyr::select(-ST, -STATE, -ST_ABBR, -STCNTY, -COUNTY, -FIPS,
                -LOCATION, -GeoLevel, -Comparison)

# Merge county, census, BRFSS (by PHR) and SVI
merged_with_svi <- county_data %>%
  left_join(texas_census, by = "County") %>%
  left_join(brfss_phr, by = "PHR") %>%
  left_join(svi, by = "County") |>
  clean_names()

write_csv(merged_with_svi, paste0(datapath, "merged_with_svi.csv"))


set.seed(100) # Set seed

# Use the in-memory merged data (already cleaned) instead of re-reading the CSV
merged <- merged_with_svi

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



# Export to csv
write_csv(train, "intermed_regress_data/train_clean.csv")
write_csv(test,  "intermed_regress_data/test_clean.csv")
saveRDS(folds, file = "intermed_regress_data/folds.rds")


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

# Export to csv
write_csv(trainclass_smote, "intermed_class_data/trainclass_smote.csv")
write_csv(testclass,  "intermed_class_data/testclass.csv")
saveRDS(folds_class, file = "intermed_class_data/folds_class.rds")

