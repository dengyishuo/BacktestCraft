dat_return_raw <- dat_style %>%
  mutate(
    date = as.Date(date),
    close = as.numeric(as.character(adjusted))
  ) %>%
  group_by(code) %>%
  arrange(date) %>%
  mutate(log_return = log(close / dplyr::lag(close))) %>%
  ungroup()

library(dplyr) # 确保 dplyr 已加载，且放在最后（避免冲突）
dat_return_clean <- dat_return_raw %>%
  dplyr::filter(!is.na(log_return), dplyr::between(log_return, -1, 1))

# 查看剔除了多少异常值
cat(
  "原始观测数:", nrow(dat_return_raw),
  "\n剔除后观测数:", nrow(dat_return_clean),
  "\n剔除比例:", round((1 - nrow(dat_return_clean) / nrow(dat_return_raw)) * 100, 2), "%"
)


# 再绘图
ggplot(
  dat_return_clean,
  aes(x = date, y = log_return, color = name, group = name)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "ETF 对数收益率对比（已剔除 |log_return|>=1 的异常值）",
    x = "日期", y = "对数收益率", color = "ETF"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")


if (!require(plotly)) install.packages("plotly")
library(plotly)
p <- ggplot(
  dat_return_clean,
  aes(x = date, y = log_return, color = name, group = name)
) +
  geom_line(linewidth = 0.5) +
  labs(
    title = "ETF 对数收益率对比（已剔除 |log_return|>=1 的异常值）",
    x = "日期", y = "对数收益率", color = "ETF"
  ) +
  theme_bw() +
  theme(plot.title = element_text(hjust = 0.5), legend.position = "bottom")

ggplotly(p, dynamicTicks = TRUE) # 转为交互图


monthly_returns <- dat_return_clean %>%
  mutate(year_month = floor_date(date, "month")) %>% # 月份分组键
  group_by(code, name, year_month) %>%
  summarise(
    monthly_log_return = sum(log_return, na.rm = TRUE), # 对数收益率累加
    .groups = "drop"
  ) %>%
  arrange(code, year_month)


fig_monthly <- plot_ly(
  data = monthly_returns,
  x = ~year_month,
  y = ~monthly_log_return,
  color = ~name,
  type = "scatter",
  mode = "lines",
  line = list(width = 1.5)
) %>%
  layout(
    title = "ETF 月对数收益率对比（剔除日度极端值后）",
    xaxis = list(title = "月份"),
    yaxis = list(title = "月对数收益率"),
    legend = list(
      title = list(text = "ETF"),
      orientation = "h",
      yanchor = "bottom",
      y = -0.2,
      xanchor = "center",
      x = 0.5
    ),
    hovermode = "x unified"
  )

fig_monthly

quarterly_returns <- dat_return_clean %>%
  mutate(year_quarter = floor_date(date, "quarter")) %>% # 转为季度起始日期（如2020-01-01）
  group_by(code, name, year_quarter) %>%
  summarise(
    quarterly_log_return = sum(log_return, na.rm = TRUE),
    .groups = "drop"
  ) %>%
  arrange(code, year_quarter)



fig_quarterly <- plot_ly(
  data = quarterly_returns,
  x = ~year_quarter,
  y = ~quarterly_log_return,
  color = ~name,
  type = "scatter",
  mode = "lines",
  line = list(width = 1.5)
) %>%
  layout(
    title = "ETF 季对数收益率对比（剔除日度极端值后）",
    xaxis = list(title = "季度"),
    yaxis = list(title = "季对数收益率"),
    legend = list(
      title = list(text = "ETF"),
      orientation = "h",
      yanchor = "bottom",
      y = -0.2,
      xanchor = "center",
      x = 0.5
    ),
    hovermode = "x unified"
  )

fig_quarterly


library(tidyr) # 用于 pivot_wider

# 将数据转为宽格式：每一行是一个 ETF，每一列是一个季度，单元格为季度对数收益率
heatmap_data <- quarterly_returns %>%
  select(name, year_quarter, quarterly_log_return) %>%
  pivot_wider(
    names_from = year_quarter,
    values_from = quarterly_log_return,
    values_fill = NA # 缺失的季度填 NA，热力图中会显示为空白或灰色
  )

# 提取行名（ETF名称）和矩阵数据
etf_names <- heatmap_data$name
return_matrix <- as.matrix(heatmap_data[, -1]) # 去掉 name 列
quarter_dates <- colnames(return_matrix) # 季度日期标签


library(plotly)

fig_heatmap <- plot_ly(
  z = return_matrix,
  x = quarter_dates,
  y = etf_names,
  type = "heatmap",
  colorscale = "RdBu", # 红蓝渐变（红高蓝低，可根据需要反转）
  reversescale = FALSE, # 若想红低蓝高可设 TRUE
  zmin = -0.3, # 固定颜色范围（根据你的数据调整）
  zmax = 0.3,
  hovertemplate = "ETF: %{y}<br>季度: %{x}<br>收益率: %{z:.3f}<extra></extra>"
) %>%
  layout(
    title = "ETF 季度对数收益率热力图",
    xaxis = list(title = "季度", tickangle = -45),
    yaxis = list(title = "ETF 标的"),
    colorbar = list(title = "对数收益率")
  )

fig_heatmap
