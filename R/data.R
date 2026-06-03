#' All-weather ETF OHLC dataset
#'
#' A dataset containing daily open, high, low, close, adjusted prices and volume
#' for 8 ETFs covering Chinese A-shares, bonds, gold, and commodities.
#'
#' @format A tibble (or data.frame) with the following columns:
#' \describe{
#'   \item{date}{Date, trading date}
#'   \item{code}{Character, stock/ETF code with exchange suffix (e.g., "510300.SS")}
#'   \item{open}{Numeric, opening price}
#'   \item{high}{Numeric, highest price of the day}
#'   \item{low}{Numeric, lowest price of the day}
#'   \item{close}{Numeric, closing price}
#'   \item{adjusted}{Numeric, adjusted closing price (forward-split and dividend adjusted)}
#'   \item{volume}{Numeric, trading volume}
#' }
#'
#' @source Data downloaded from FactorCraft package via `get_data()`.
#' @usage data("all_weather")
#' @docType data
#' @keywords datasets
#' @name all_weather
#' @examples
#' \dontrun{
#' data("all_weather")
#' head(all_weather)
#' }
NULL

#' Style ETF OHLC dataset
#'
#' A dataset containing daily open, high, low, close, adjusted prices and volume
#' for 13 style ETFs (value, growth, quality, dividend, etc.) traded on Chinese
#' exchanges.
#'
#' @format A tibble (or data.frame) with the following columns:
#' \describe{
#'   \item{date}{Date, trading date}
#'   \item{code}{Character, ETF code with exchange suffix (e.g., "563020.SS")}
#'   \item{open}{Numeric, opening price}
#'   \item{high}{Numeric, highest price of the day}
#'   \item{low}{Numeric, lowest price of the day}
#'   \item{close}{Numeric, closing price}
#'   \item{adjusted}{Numeric, adjusted closing price (forward-split and dividend adjusted)}
#'   \item{volume}{Numeric, trading volume}
#' }
#'
#' @source Data downloaded from FactorCraft package via `get_data()`.
#' @usage data("style")
#' @docType data
#' @keywords datasets
#' @name style
#' @examples
#' \dontrun{
#' data("style")
#' head(style)
#' }
NULL
