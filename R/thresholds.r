#' Rate Check
#'
#' Compare the rate of change of a variable against a threshold.
#'
#' @param x A vector of values.
#' @param tm A vector of timestamps.
#' @param limit The threshold to compare to.
#' @return Logical `TRUE` if threshold is exceeded, `FALSE` otherwise.
#'
#' @importFrom dplyr lag
#' @importFrom lubridate as.duration
#' @export
check_rate = function(x, tm, limit) {
  dx = (x - lag(x))
  dt = as.numeric(as.duration(tm - lag(tm)))

  any(dx / dt > limit, na.rm = TRUE)
}

#' Limit Check
#'
#' Compare a variable against a maximum limit.
#'
#' @inheritParams check_rate
#' @return Logical `TRUE` if threshold is exceeded, `FALSE` otherwise.
#'
#' @export
check_limit = function(x, limit) {
  any(max(x, na.rm = TRUE) >= limit, na.rm = TRUE)
}

check_surge = function(x, limit) {
  any(x >= limit)
}