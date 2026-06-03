# ETF 代码与名称对应表
stock_df <- data.frame(
  code = c(
    "510300.SS",
    "512100.SS",
    "512890.SS",
    "511130.SS",
    "511260.SS",
    "511010.SS",
    "518880.SS",
    "510170.SS"
  ),
  name = c(
    "沪深300ETF",
    "中证1000",
    "红利低波",
    "30年期国债",
    "10年期国债",
    "5年期国债",
    "黄金ETF",
    "商品ETF"
  ),
  stringsAsFactors = FALSE
)

library(FactorCraft)
dat <- get_data(stock_df, start_date = "2020-01-01", end_date = "2026-05-01")

dat_with_signal <- add_signal(dat, signal_type = "constant")

dat_with_weight <- add_fixed_weight(dat_with_signal,
  signal_col = "signal_constant_1",
  fixed_weights = c(0.15, 0.08, 0.07, 0.2, 0.2, 0.15, 0.075, 0.075),
  strict_check = FALSE
)


# ==============================================
# 1. backtest 函数
# ==============================================
bt_result <- backtest(
  # 核心基础参数
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

  # 调仓参数
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",

  # 价格参数
  exec_price_col = "close",
  eval_price_col = "adjusted",

  # 风险控制
  enable_stop_loss = FALSE,

  # 输出格式
  output_type = "tibble"
)

# ==============================================
# 3. run_backtest_final 函数
# ==============================================
res <- run_backtest_final(
  # 核心基础参数
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

  # 调仓参数
  rebalance_mode = "calendar",
  rebalance_cycle = "quarterly",
  weight_change_threshold = 0.01,

  # 价格参数
  exec_price_col = "close",
  eval_price_col = "adjusted",

  # 风险控制
  enable_component_stop_loss = FALSE,
  enable_portfolio_stop_loss = FALSE,
  enable_component_take_profit = FALSE,
  enable_portfolio_take_profit = FALSE,

  # 输出格式
  output_type = "tibble"
)


##########################################################
################ 风格轮动ETF组合
##########################################################


# ETF 代码与名称对应表
stock_df <- data.frame(
  code = c(
    "563020.SS",
    "515180.SS",
    "512100.SS",
    "159531.SZ",
    "159259.SZ",
    "159967.SZ",
    "588020.SS",
    "562310.SS",
    "562520.SS",
    "159606.SZ",
    "159209.SZ",
    "515960.SS",
    "560500.SS"
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

library(FactorCraft)
dat_style <- get_data(stock_df, start_date = "2020-01-01", end_date = "2026-05-31")

dat_style_with_indicator <- add_risk_adj_mom(dat_style, close_col = "adjusted")

dat_style_with_signal <- add_rank_signal(dat_style_with_indicator, rank_col = "ram_20", top_n = 3)

# 生成权重
dat_style_with_weight <- add_norm_weight(
  df = dat_style_with_signal, # 你的输入数据
  weight_col = "ram_20", # 用什么指标加权（RAM动量）
  signal_col = "signal_ram_20_top3", # 只给选中的3只ETF加权
  norm_method = "softmax" # linear = 等权重（推荐）
)


# ==============================================
# 3. run_backtest_final 函数
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
