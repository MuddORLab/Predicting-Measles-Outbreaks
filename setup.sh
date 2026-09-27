#!/usr/bin/env bash
set -e

echo "Setting up Python environment..."
conda env create -f environment.yml
conda run -n measles playwright install chromium

echo "Setting up R environment..."
Rscript setup_R.R

echo "Setting up Census API key..."
if [ -f .Renviron ] && grep -q "CENSUS_API_KEY" .Renviron; then
  echo ".Renviron has the key already."
else
  echo "Need a free Census API key: https://api.census.gov/data/key_signup.html"
  read -rsp "Paste the key and press Enter: " KEY
  echo
  if [ -n "$KEY" ]; then
    echo "CENSUS_API_KEY=$KEY" >> .Renviron
    echo "Successfully written to .Renviron"
  else
    echo "No key provided. Please create .Renviron manually and add the following line:"
    echo "CENSUS_API_KEY=<your_key_placeholder>"
  fi
fi