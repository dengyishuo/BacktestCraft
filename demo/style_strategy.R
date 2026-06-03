# ==============================================
# Demo: Style ETF Momentum Strategy
# Using built-in dataset 'style'
# ==============================================

# Load required packages
library(FactorCraft)

# ==============================================
# 1. Load built-in style dataset and rename to dat_style
# ==============================================
data("style", package = "FactorCraft")
dat_style <- style

# ==============================================
# 2. Add risk-adjusted momentum (RAM) indicator
# ==============================================
dat_style_with_indicator <- add_risk_adj_mom(dat_style, close_col = "adjusted")

# ==============================================
# 3. Generate ranking signal: select top 3 ETFs daily based on ram_20
# ==============================================
dat_style_with_signal <- add_rank_signal(dat_style_with_indicator, rank_col = "ram_20", top_n = 3)

# ==============================================
# 4. Generate dynamic weights using softmax on ram_20 among selected assets
# ==============================================
dat_style_with_weight <- add_norm_weight(
  df = dat_style_with_signal,
  weight_col = "ram_20",
  signal_col = "signal_ram_20_top3",
  norm_method = "softmax"
)

# ==============================================
# 5. Run backtest with hybrid rebalancing (monthly + 5% drift)
# ==============================================
res_style <- run_backtest_final(
  df = dat_style_with_weight,
  weight_col = "weight_ram_20_signal_ram_20_top3",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  skip_suspended = TRUE,
  rebalance_mode = "hybrid",
  rebalance_cycle = "monthly",
  weight_change_threshold = 0.05,
  exec_price_col = "close",
  eval_price_col = "adjusted",
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,
  output_type = "tibble"
)

# ==============================================
# 6. View results
# ==============================================
print(res_style$config)
tail(res_style$equity_curve)
head(res_style$transactions)
