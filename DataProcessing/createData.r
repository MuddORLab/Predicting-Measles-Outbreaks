# locate proj and load packages 
library(here)
library(dplyr)
library(readr)
library(stringr)
library(viridis)
library(tidycensus)
library(tidyverse)
here::i_am("DataProcessing/CreateData.r")

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

df <- cve %>%
  inner_join(
    outbreak,
    by = "County",
    suffix = c("_cve", "_out")
  )

df2025 <- df2025 %>%
  left_join(enrollment, by = "County") %>%
  left_join(population, by = "County") %>%
  inner_join(phr_conv, by = "County") %>%
  left_join(texas_counties, by = "County") %>%
  left_join(svi, by = "County")

write_csv(df2025, here("data", "datasets", "Clustering_Dataset.csv"))

# merge with BRFSS data
brfss <- read_csv(here("data", "raw", "brfss.csv"))

finaldf <- df2025 |>
  left_join(brfss, by = c("phr" = "PHR"))

write_csv(finaldf, here("data", "datasets", "FullData.csv"))