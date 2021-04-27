#' Get Station Data
#'
#' Get station data from CDEC.
#'
#' @param stations The CDEC station IDs to query.
#' @param sensors The CDEC sensor numbers to query.
#' @return A dataframe.
#'
#' @importFrom dplyr transmute filter left_join between
#' @importFrom rlang .data
#' @importFrom stringr str_detect str_c
#' @importFrom lubridate duration now force_tz with_tz
#'   as_date floor_date ceiling_date
#' @importFrom cder cdec_query
#' @keywords internal
get_station_data = function(stations, sensors, por, prog, db) {
  # set start and end times
  end = force_tz(now(), Sys.timezone())
  end = with_tz(end, "Etc/GMT+8")
  start = end - duration(por)
  # CDEC only handles dates
  start.date = as_date(floor_date(start, "days"))
  end.date = as_date(ceiling_date(end, "days"))
  # Get data and filter by time period
  transmute(
    filter(
      cdec_query(stations, sensors, "E", start.date, end.date),
      between(.data$DateTime, start, end)
    ),
    Station = .data$StationID,
    Datetime = .data$DateTime,
    Stage = .data$Value
  )
}

#' Get CNRFC Forecast
#'
#' Get the CNRFC Forecast for a location.
#'
#' @param location The location for which to retrieve the forecast.
#'   Default is Rio Vista "RVBC1".
#'
#' @importFrom dplyr select
#' @importFrom readr read_csv cols_only col_number col_datetime
#'   col_character
#' @export
get_cnrfc_forecast = function(location = "RVBC1") {
  start = force_tz(now(), Sys.timezone())
  start = with_tz(start, "Etc/GMT+8")
  forecast.url = glue("https://www.cnrfc.noaa.gov/graphicalRVF_csv.php?id={location}")
  forecast = read_csv(forecast.url, na = c("", "N/A"),
    col_types = cols_only(
    "Date/Time" = col_datetime(format = "%m/%d/%Y %H %p "),
    "Stage (Feet)" = col_number()
    )
  )
  forecast = mutate(forecast, Station = "RVB")
  filter(
    select(forecast, Station = "Station", Datetime = "Date/Time",
      Stage = "Stage (Feet)"),
    .data$Datetime >= start
  )
}

#' Check the Roaring River Distribution System
#'
#' Check stages in the Roaring River Distribution System
#' recorded on CDEC and send an email alert if stages exceed
#' the specified threshold.
#'
#' @param stage.threshold The threshold stage to trigger an alert.
#'   default is 4.6 ft NAVD88.
#' @param rate.threshold The threshold rate to trigger an alert,
#'   in units of ft/s. Default is 0.3 ft in 15 minutes.
#' @param outside.stage.threshold The threshold stage outside of the
#'   system required to trigger a rate-based alert. default is
#'   7.2 ft NAVD88.
#' @param surge.threshold The threshold stage prediction at Rio Vista
#'   Bridge required to trigger a storm surge alert. default is 8.0 ft
#'   NAVD88.
#' @param por The period of record to use for check. Default is to use
#'   previous 6 hours of data.
#' @param alert If `TRUE`, email alerts will be sent if thresholds
#'   are triggered.
#' @param stage.template The YAML template to use for the stage alerts.
#' @param rate.template The YAML template to use for the rate alerts.
#' @param surge.template The YAML template to use for the storm surge
#'   alerts.
#' @param verbose If `TRUE`, display messages on console.
#' @return `TRUE` if message is sent, `FALSE` if thresholds are not
#'   exceeded.
#'
#' @importFrom dplyr mutate slice lag filter first
#' @importFrom rlang .data
#' @export
check_rrds = function(stage.threshold = 4.6,
  rate.threshold = 3.6e-5, outside.stage.threshold = 7.2,
  surge.threshold = 8.0, por = "6 hours",
  alert = TRUE, verbose = FALSE,
  stage.template = default.stage.template,
  rate.template = default.rate.template,
  surge.template = default.surge.template) {
  # get data
  data = get_station_data(c("PEL", "ROR", "MSL"), c(1L, 6L), por)
  inside.data = filter(data, .data$Station %in% c("PEL", "ROR"))
  outside.data = filter(data, .data$Station %in% c("MSL"))
  surge.data = get_cnrfc_forecast("RVBC1")
  if (check_limit(inside.data$Stage, stage.threshold)) {
    # stage threshold triggered
    trigger = TRUE
    alert.plot = make_figure(inside.data, stage.threshold)
    alert.data = mutate(slice(inside.data, which.max(.data$Stage)),
      Threshold = stage.threshold)
    alert.contents = alert_from_template(as.list(alert.data),
      stage.template, alert.plot)
  } else if (check_rate(inside.data$Stage, inside.data$Datetime, rate.threshold) &
             check_limit(outside.data$Stage, outside.stage.threshold)) {
    # rate threshold triggered
    trigger = TRUE
    alert.plot = make_figure(inside.data, stage.threshold)
    alert.data = mutate(
      slice(inside.data, rep(which.max((.data$Stage - lag(.data$Stage)) /
        as.numeric(.data$Datetime - lag(.data$Datetime))), 2) + c(-1, 0)),
      Threshold = rate.threshold
    )
    alert.contents = alert_from_template(as.list(alert.data),
      rate.template, alert.plot)
  } else if (check_surge(surge.data$Stage, surge.threshold)) {
    # flood surge threshold triggered
    trigger = TRUE
    alert.plot = make_figure(surge.data, surge.threshold)
    alert.data = mutate(
      slice(surge.data, first(which(.data$Stage >= surge.threshold))),
      Threshold = surge.threshold)
    alert.contents = alert_from_template(as.list(alert.data),
      surge.template, alert.plot)
  }
  else {
    # no thresholds triggered
    trigger = FALSE
  }
  # console message
  if (verbose) {
    if (trigger) {
      message(alert.contents$Body)
    } else {
      message("No alerts triggered.")
    }
  }
  # send alert
  if (alert & trigger) {
    send_alert(alert.contents)
  } else {
    invisible(FALSE)
  }
}
