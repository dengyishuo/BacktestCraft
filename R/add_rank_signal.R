#' Rank-based signal generation function
#'
#' Generate 0/1 signals by ranking specified indicators within each trading day.
#' Select TOP N or BOTTOM N stocks based on ranking order.
#' Optionally filter stocks by a minimum threshold on the ranking column before ranking.
#' Fully compatible with quantitative strategy long-format data structure.
#'
#' @param df Data frame in long format, must contain 'date' and 'code' columns
#' @param rank_col Character string of column name to rank (e.g., "mom_20", "vol_60")
#' @param top_n Integer, number of stocks to select per day, default 2
#' @param rank_order Ranking order: "desc" for descending (TOP), "asc" for ascending (BOTTOM)
#' @param tie_method Method for handling ties: "min", "max", or "average", default "min"
#' @param rank_threshold Numeric value; only stocks with rank_col > rank_threshold are considered for ranking.
#'   If NULL (default), no threshold filtering is applied.
#' @param signal_name Character, output signal column name. Auto-generated if NULL
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Data frame with an appended 0/1 signal column (1 = selected, 0 = not selected)
#'   in the specified output format
#'
#' @importFrom dplyr group_by mutate ungroup select summarise
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # Select TOP 2 stocks by mom_20 (descending order)
#' df <- add_rank_signal(df, rank_col = "mom_20", top_n = 2)
#'
#' # Select BOTTOM 3 stocks by vol_60 (ascending order)
#' df <- add_rank_signal(df, rank_col = "vol_60", top_n = 3, rank_order = "asc")
#'
#' # Select TOP 5 stocks with mom_20 > 0.5 (filter before ranking)
#' df <- add_rank_signal(df, rank_col = "mom_20", top_n = 5, rank_threshold = 0.5)
#'
#' # Return as base data.frame instead of tibble
#' df <- add_rank_signal(df, rank_col = "mom_20", top_n = 5, output_type = "data.frame")
#' }
add_rank_signal <- function(
  df,
  rank_col,
  top_n = 2,
  rank_order = "desc",
  tie_method = "min",
  rank_threshold = NULL,
  signal_name = NULL,
  output_type = c("tibble", "data.frame")
) {
  # 1. Input validation
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Data must contain 'date' and 'code' columns!")
  }
  if (!rank_col %in% colnames(df)) {
    stop("Ranking column not found: ", rank_col)
  }

  # 2. Auto-generate signal column name
  if (is.null(signal_name)) {
    order_text <- ifelse(rank_order == "desc", "top", "bottom")
    if (!is.null(rank_threshold)) {
      signal_name <- paste0("signal_", rank_col, "_", order_text, top_n, "_gt", rank_threshold)
    } else {
      signal_name <- paste0("signal_", rank_col, "_", order_text, top_n)
    }
  }

  # 3. Core ranking logic with optional threshold filter
  result <- df %>%
    dplyr::group_by(date) %>%
    dplyr::mutate(
      # Clean the ranking column: NA/Inf -> NA
      rank_val_raw = ifelse(is.na(!!sym(rank_col)) | is.infinite(!!sym(rank_col)), NA_real_, !!sym(rank_col)),

      # Apply threshold filter: only keep rows where rank_val_raw > rank_threshold (if threshold provided)
      pass_filter = if (!is.null(rank_threshold)) rank_val_raw > rank_threshold else TRUE,
      pass_filter = ifelse(is.na(pass_filter), FALSE, pass_filter),

      # Value used for ranking: set to NA for rows that fail the filter
      rank_val = ifelse(pass_filter, rank_val_raw, NA_real_),

      # Compute rank (descending or ascending) with NA placed last
      .rank = if (rank_order == "desc") {
        rank(-rank_val, na.last = TRUE, ties.method = tie_method)
      } else {
        rank(rank_val, na.last = TRUE, ties.method = tie_method)
      },

      # Generate signal: 1 if passes filter and rank <= top_n
      !!signal_name := as.integer(pass_filter & .rank <= top_n)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-rank_val_raw, -rank_val, -.rank, -pass_filter)

  # 4. Ensure any NA signals are set to 0 (though as.integer already does this, safe to double-check)
  result[[signal_name]] <- ifelse(is.na(result[[signal_name]]), 0, result[[signal_name]])

  # 5. Compute daily average selected count (for informative message)
  daily_count <- result %>%
    dplyr::group_by(date) %>%
    dplyr::summarise(n = sum(!!sym(signal_name), na.rm = TRUE), .groups = "drop")
  avg_selected <- round(mean(daily_count$n, na.rm = TRUE), 2)

  message(
    "Generated signal column: ", signal_name,
    " | Daily average selected: ", avg_selected, " (target top_n = ", top_n, ")"
  )

  # 6. Output format conversion
  if (output_type[1] == "tibble") {
    result <- tibble::as_tibble(result)
  }

  return(result)
}
