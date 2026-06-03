#' Generate signal for consecutive days meeting a condition
#'
#' Create a 0/1 signal that becomes 1 when a condition has been met for N
#' consecutive trading days (within each asset). Useful for trend confirmation
#' or entry filters like "close > SMA(20) for 3 consecutive days".
#'
#' @param df Data frame in long format, must contain 'date', 'code', and the
#'   condition column (or a pre-computed signal column)
#' @param condition_col Character string of a column containing 0/1 condition
#'   values (e.g., from \code{add_signal}).
#' @param n_consecutive Integer, number of consecutive days required
#' @param signal_name Character, output signal column name. Auto-generated if NULL.
#' @param output_type Output format: "tibble" (default) or "data.frame"
#'
#' @return Original data frame with an appended signal column (1 when condition
#'   has been true for n_consecutive days, 0 otherwise)
#'
#' @importFrom dplyr group_by mutate lag ungroup
#' @importFrom rlang .data !! sym :=
#' @export
#'
#' @examples
#' \dontrun{
#' # First create a base condition: close > SMA(20)
#' df <- add_signal(df,
#'   indicator_cols = "close", signal_type = "threshold",
#'   threshold = "SMA_20", compare_op = ">", signal_name = "above_sma"
#' )
#' # Then require 3 consecutive days above SMA
#' df <- add_consecutive_signal(df, condition_col = "above_sma", n_consecutive = 3)
#' }
add_consecutive_signal <- function(
  df,
  condition_col,
  n_consecutive = 2,
  signal_name = NULL,
  output_type = c("tibble", "data.frame")
) {
  # Input validation
  if (!all(c("date", "code") %in% colnames(df))) {
    stop("Data must contain 'date' and 'code' columns!")
  }
  if (!condition_col %in% colnames(df)) {
    stop("Condition column not found: ", condition_col)
  }
  if (n_consecutive < 1) stop("n_consecutive must be >= 1")

  output_type <- match.arg(output_type)

  # Auto-generate name
  if (is.null(signal_name)) {
    signal_name <- paste0("signal_", condition_col, "_consecutive", n_consecutive)
  }

  result <- df %>%
    dplyr::group_by(.data$code) %>%
    dplyr::arrange(.data$date, .by_group = TRUE) %>%
    dplyr::mutate(
      # Convert condition column to integer (0/1)
      cond = as.integer(!!sym(condition_col) > 0),
      # Cumulative sum of condition resets when condition is 0
      streak = {
        rle_cond <- rle(.data$cond)
        rep(rle_cond$lengths, rle_cond$lengths) * .data$cond
      },
      !!signal_name := as.integer(.data$streak >= n_consecutive)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::select(-.data$cond, -.data$streak)

  # Message
  message(
    " Generated consecutive signal column: ", signal_name,
    " (requires ", n_consecutive, " consecutive days)"
  )

  # Output format
  if (output_type == "tibble") {
    result <- tibble::as_tibble(result)
  }
  return(result)
}
