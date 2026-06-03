# File: data-raw/create_all_weather.R
# Purpose: Fetch raw OHLC data from FactorCraft and save as built-in dataset 'all_weather'
# Execution: Run Rscript data-raw/create_all_weather.R in package root, or source("data-raw/create_all_weather.R")

library(FactorCraft)

# ==============================================
# 1. Define asset codes and names (8 ETFs)
# ==============================================
etf_data <- data.frame(
  code = c(
    "510300.SS", "512100.SS", "512890.SS",
    "511130.SS", "511260.SS", "511010.SS",
    "518880.SS", "510170.SS"
  ),
  name = c(
    "沪深300ETF", "中证1000", "红利低波",
    "30年期国债", "10年期国债", "5年期国债",
    "黄金ETF", "商品ETF"
  ),
  stringsAsFactors = FALSE
)

# ==============================================
# 2. Fetch raw OHLC data (no signals or weights added)
# ==============================================
cat("Fetching raw OHLC data from FactorCraft...\n")
all_weather <- get_data(etf_data, start_date = "2020-01-01", end_date = "2026-05-01")
cat("Data fetched successfully. Total rows:", nrow(all_weather), "\n")

# ==============================================
# 3. Save as built-in dataset
# ==============================================
# Ensure data/ directory exists
if (!dir.exists("data")) dir.create("data")

# Save data (using xz compression to reduce size)
usethis::use_data(all_weather, overwrite = TRUE, compress = "xz")

cat("Built-in dataset saved to data/all_weather.rda\n")
