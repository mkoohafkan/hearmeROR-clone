#' Loop RRDS Messaging
#'
#' Run `check_rrds` in a perpetual loop.
#'
#' @param ... Arguments to pass to `check_rrds`
#' @param check.interval Interval of time to wait between
#'   consecutive checks.
#' @param alert.interval Interval of time to wait between
#'   consecutive alerts.
#' @param error.template The YAML template to use for
#'   constructing notifications of alert system failure.
#'
#' @importFrom utils flush.console
#' @importFrom lubridate duration
#' @export
loop_rrds = function(..., check.interval = "1 hour",
  alert.interval = "1 day",
  error.template = default.error.template) {
  consecutive.errors = 0L
  alerted = FALSE
  while (TRUE) {
    tryCatch(
      expr = {
        time.since.last.alert = Sys.time() - last.alert
        alert.ok = time.since.last.alert > duration(alert.interval)
        if (check_rrds(..., alert = alert.ok)) {
          last.alert <<- Sys.time()
          alerted = TRUE
        }
        consecutive.errors <- 0L
      },
      error = function(e) {
        consecutive.errors <<- consecutive.errors + 1L
        warning(paste(e), immediate. = TRUE, call. = FALSE)
        if (((consecutive.errors - 1) %% 4) == 0L) {
          alert_from_template(list(tries = consecutive.errors),
            template.file = error.template)
        }
      },
      finally = {
        if (alerted) {
          message(glue("Alert sent at {last.alert}"))
          alerted = FALSE
        } else {
          message(glue("Check complete at {Sys.time()}"))
        }
        flush.console()
        Sys.sleep(as.numeric(duration(check.interval)))
      }
    )
  }
}
