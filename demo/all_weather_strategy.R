# File: demo/backtest_demo.R
# Purpose: Replicate backtest workflow on all_weather data,
#          comparing backtest and run_backtest_final

# Load package
library(eBacktestCraft)

# Load built-in raw data
data("all_weather")

# ==============================================
# 1. Add constant signal and fixed weights
# ==============================================
cat("\n=== Adding constant signal and fixed weights ===\n")
dat_with_signal <- add_signal(all_weather, signal_type = "constant")
dat_with_weight <- add_fixed_weight(
  dat_with_signal,
  signal_col = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check = FALSE
)
cat("Data preparation completed\n")

# ==============================================
# 2. Run original backtest
# ==============================================
cat("\n=== Running backtest (original version) ===\n")
bt_result <- backtest(
  df = dat_with_weight,
  weight_col = "weight_fixed_signal_constant_1",
  signal_col = "signal_constant_1",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",
  exec_price_col = "close",
  eval_price_col = "adjusted",
  enable_stop_loss = FALSE,
  output_type = "tibble"
)

# ==============================================
# 3. Run enhanced run_backtest_final
# ==============================================
cat("\n=== Running run_backtest (enhanced version) ===\n")
res <- run_backtest(
  df = dat_with_weight,
  weight_col = "weight_fixed_signal_constant_1",
  init_capital = 100000,
  fee_rate = 0.0003,
  stamp_tax = 0.0005,
  slippage_rate = 0.001,
  lot_size = 100,
  min_weight = 1e-6,
  single_max_weight = 0.95,
  global_max_hold_pct = 1.0,
  skip_suspended = TRUE,
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",
  weight_change_threshold = 0.01,
  exec_price_col = "close",
  eval_price_col = "adjusted",
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,
  output_type = "tibble"
)

# ==============================================
# 4. Print comparison summary
# ==============================================
cat("\n=== Results comparison ===\n")
final_backtest <- tail(bt_result$equity_curve$total_asset, 1)
final_final <- tail(res$equity_curve$total_asset, 1)
ret_backtest <- (final_backtest / 100000 - 1) * 100
ret_final <- (final_final / 100000 - 1) * 100

cat(sprintf("backtest final asset: %.2f, total return: %.2f%%\n", final_backtest, ret_backtest))
cat(sprintf("run_backtest_final final asset: %.2f, total return: %.2f%%\n", final_final, ret_final))
cat(sprintf("backtest number of trades: %d\n", nrow(bt_result$transactions)))
cat(sprintf("run_backtest_final number of trades: %d\n", nrow(res$transactions)))
