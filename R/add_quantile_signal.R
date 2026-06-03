#' Generate signal by selecting top/bottom quantile of stocks each day
#'
#' Within each trading day, rank the indicator and select stocks that fall
#' above a given quantile (e.g., top 20%) or below a quantile (bottom 20%).
#' This is a flexible alternative to \code{add_rank_signal} when you want
#' to select a variable number of stocks (percentage-based) rather than a fixed count.
#'
#' @param df Data frame in long format, must contain 'date' and 'code' columns
#' @param rank_col Character string of column name to rank (e.g., "mom_20")
#' @param quantile Numeric between 0 and 1. For top selection, the top `quantile`
#'   fraction of stocks (e.g., 0.2 = top 20\%). For bottom selection, use `select = "bottom"`.
#' @param select Character: "top" (higher values are better) or "bottom"
#' @param signal_name Output signal column name. Auto-generated if NULL.
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with an appended 0/1 signal column (1 = selected by quantile)
#'
#' @importFrom dplyr group_by mutate ungroup summarise
#' @importFrom stats quantile
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Select top 20% stocks by momentum each day
#' df <- add_quantile_signal(df, rank_col = "mom_20", quantile = 0.2, select = "top")
#'
#' # Select bottom 10% by volatility
#' df <- add_quantile_signal(df, rank_col = "vol_60", quantile = 0.1, select = "bottom")
#' }
add_quantile_signal <- function(
  df,
  rank_col,
  quantile = 0.2,
  select = c("top", "bottom"),
  signal_name = NULL,
  output_type = c("tibble", "data.frame")
) {
  # Input validation
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Data must contain 'date' and 'code' columns!")
  }
  if (!rank_col %in% colnames(df)) {
    stop("Rank column not found: ", rank_col)
  }
  if (quantile <= 0 || quantile >= 1) {
    stop("quantile must be between 0 and 1 (exclusive)")
  }
  select <- match.arg(select)
  output_type <- match.arg(output_type)

  # Auto-generate name
  if (is.null(signal_name)) {
    select_text <- ifelse(select == "top", "top", "bottom")
    signal_name <- paste0("signal_", rank_col, "_", select_text, "_q", quantile * 100)
  }

  result <- df %>%
    dplyr::group_by(.data$date) %>%
    dplyr::mutate(
      rank_val = !!sym(rank_col),
      # Compute the quantile threshold per day
      q_thresh = if (select == "top") {
        stats::quantile(.data$rank_val, probs = 1 - quantile, na.rm = TRUE, type = 8)
      } else {
        stats::quantile(.data$rank_val, probs = quantile, na.rm = TRUE, type = 8)
      },
      # Generate signal
      !!signal_name := dplyr::case_when(
        select == "top" ~ as.integer(.data$rank_val >= .data$q_thresh & !is.na(.data$rank_val)),
        select == "bottom" ~ as.integer(.data$rank_val <= .data$q_thresh & !is.na(.data$rank_val))
      )
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$rank_val, -.data$q_thresh)

  # Replace NA (if any) with 0
  result[[signal_name]] <- ifelse(is.na(result[[signal_name]]), 0, result[[signal_name]])

  # Compute average number selected per day for info
  daily_counts <- result %>%
    dplyr::group_by(.data$date) %>%
    dplyr::summarise(n = sum(!!sym(signal_name), na.rm = TRUE), .groups = "drop")
  avg_selected <- round(mean(daily_counts$n, na.rm = TRUE), 2)
  pct_selected <- quantile * 100

  message(
    " Generated quantile signal column: ", signal_name,
    " (select ", select, " ", pct_selected, "% (", avg_selected, " avg stocks/day))"
  )

  if (output_type == "tibble") {
    result <- tibble::as_tibble(result)
  }
  return(result)
}
