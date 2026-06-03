# File: data-raw/create_style.R
# Purpose: Fetch raw OHLC data for style ETFs from FactorCraft and save as built-in dataset 'style'
# Execution: Run Rscript data-raw/create_style.R in package root, or source("data-raw/create_style.R")

library(FactorCraft)

# ==============================================
# 1. Define asset codes and names (13 style ETFs)
# ==============================================
style_df <- data.frame(
  code = c(
    "563020.SS", # E Fund Low Volatility Dividend ETF
    "515180.SS", # E Fund Dividend ETF
    "512100.SS", # Southern CSI 1000 ETF
    "159531.SZ", # Southern CSI 2000 ETF
    "159259.SZ", # E Fund Growth ETF
    "159967.SZ", # ChinaAMC ChiNext Growth ETF
    "588020.SS", # E Fund STAR 50 Growth ETF
    "562310.SS", # Yinhua CSI 300 Growth ETF
    "562520.SS", # ChinaAMC CSI 1000 Growth ETF
    "159606.SZ", # E Fund CSI 500 Growth ETF
    "159209.SZ", # China Merchants Dividend Quality ETF
    "515960.SS", # CICC Quality ETF
    "560500.SS" # Panyang 500 Quality Growth ETF
  ),
  name = c(
    "红利低波ETF易方达",
    "红利ETF易方达",
    "中证1000ETF南方",
    "中证2000ETF南方",
    "成长ETF易方达",
    "创业板成长ETF华夏",
    "科创成长ETF易方达",
    "沪深300成长ETF银华",
    "中证1000成长ETF华夏",
    "中证500成长ETF易方达",
    "红利质量ETF招商",
    "质量ETF中金",
    "500质量成长ETF鹏扬"
  ),
  stringsAsFactors = FALSE
)

# ==============================================
# 2. Fetch raw OHLC data (no signals or weights added)
# ==============================================
cat("Fetching raw OHLC data for style ETFs from FactorCraft...\n")
style <- get_data(style_df, start_date = "2020-01-01", end_date = "2026-05-31")
cat("Data fetched successfully. Total rows:", nrow(style), "\n")

# ==============================================
# 3. Save as built-in dataset
# ==============================================
# Ensure data/ directory exists
if (!dir.exists("data")) dir.create("data")

# Save data (using xz compression to reduce size)
usethis::use_data(style, overwrite = TRUE, compress = "xz")

cat("Built-in dataset saved to data/style.rda\n")
