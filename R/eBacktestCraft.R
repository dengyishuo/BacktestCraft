#' @keywords internal
"_PACKAGE"

## usethis namespace: start
#' @import dplyr
#' @import lubridate
#' @import zoo
#' @import ggplot2
#' @importFrom rlang .data !! sym :=
#' @importFrom stats na.omit sd setNames
#' @importFrom utils tail
NULL
## usethis namespace: end

# Declare global variables
utils::globalVariables(c(
  "code", "weight", "fixed_weight", "selected_codes",
  "high", "low", "adjusted", "volume"
))
