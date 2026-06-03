# This file declares global variables to suppress R CMD check NOTES
# It is intentionally free of roxygen2 tags to avoid parsing issues.

utils::globalVariables(c(
  "g", "w_prev", "drift", "trigger", "daily_return",
  "rank_val_raw", "pass_filter", "rank_val", ".rank",
  "median"
))
