#' Make Figure
#' Make a plot of RRDS stages from CDEC data.
#'
#' @param d The dataset construct the plot from.
#' @param threshold The threshold stage, plotted as 
#'   as a horizontal line.
#' @return a `ggplot` object.
#'
#' @importFrom rlang .data
#' @importFrom ggplot2 ggplot aes annotate geom_line theme_bw 
#'   scale_x_datetime scale_color_brewer scale_y_continuous
#' @keywords internal
make_figure = function(d, threshold) {
  ggplot(d) + theme_bw() +
    aes(x = .data$Datetime, y = .data$Stage, color = .data$Station) +
    geom_line(size = 1) +
    scale_x_datetime(NULL, date_labels = "%d-%b\n%H:%M",
      timezone = "US/Pacific") +
    scale_color_brewer(NULL, palette = "Set2") +
    scale_y_continuous("Stage (ft NAVD88)") +
    annotate("segment", x = min(d$Datetime), xend = max(d$Datetime),
      y = threshold, yend = threshold, color = "red", size = 1,
      linetype = "longdash") 
}
