#' Function for plotting beautiful candle-charts in ggplot
#' @param data data frame containing data for plotting
#' @param na.rm a logical indicating whether NA values should be stripped
#' before plotting
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param x.interval numeric used to set number of breaks when scaling the x variable
#' @param dc.lines character specifying if "both", "none", the "high", or the "low" Donchian
#' channel line should be used
#' @param dc.window integer specifying the with of the time window used in the Donchian
#' channel calculation
#' @param output.length integer. Specifying the number of output observations
#' @details
#' No further comments...
#' @examples
#' n <- 30
#' data <- data.frame(open = rnorm(n), high = rnorm(n)+1, low = rnorm(n)-1, close = rnorm(n))
#' rownames(data) <- Sys.Date() + 1:n
#' candle_chart(data, na.rm = T, title = "Example of candle-chart - might look funny due to random numbers",
#'              sub.title = NULL, y.title = "prices", dc.lines = "both", base.size = 14, x.interval = 5)
#' @export

candle_chart <- function(data, na.rm = FALSE, title = NULL, sub.title = NULL, y.title = NULL,
                         x.title = NULL, x.interval = NULL, lwd = 0.2, bar.width = 1,
                         dc.lines = "both", dc.window = 10, output.length = NULL, ...) {

  if (na.rm) data <- data[complete.cases(data),]

  # stop if input object is not a data.frame and x and y variables not specified
  if (!is.data.frame(data)) stop("Input object has to be data.frame")

  # if x and y axis titles are not NULL include line break
  if(!is.null(x.title)) x.title <- paste("\n", x.title)
  if(!is.null(y.title)) y.title <- paste(y.title, "\n")

  # stop if data does not contain 4 columns with the names "open", "high", "low", "close"
  if (any( !(c("open", "high", "low", "close") %in% dimnames(data)[[2]]) )) {
    stop("data must contain the four column 'open', 'high', 'low', and 'close'")
  }

  # define chart title object
  if (!is.null(title) & !is.null(sub.title)) {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  } else if (!is.null(title) & is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- NULL
  }

  # create info for plot
  data <- data.frame(date = as.Date(rownames(data)), data)
  data$chg <- ifelse(data$close > data$open, "up", "dn")
  data$width <- bar.width
  data$flat_bar <- data[, "high"] == data[, "low"]

  # create Donchian Channels
  if (dc.lines != "none") {
    dc <- TTR::DonchianChannel(data[, c("high", "low")], n = dc.window)
  }

  # if output length is not NULL
  if (!is.null(output.length)) {
    data <- tail(data, output.length)
    if (dc.lines != "none") {
      dc <- tail(dc, output.length)
    }
  }

  # configure x-labels
  if (!is.null(x.interval)) {
      date.seq <- seq(1, length(data$date), floor(length(data$date) / x.interval))
  }
  date.format <- "%Y-%m-%d"

  # creating chart
  g <- ggplot(data, aes(x = date)) +
              geom_linerange(aes(ymin = low, ymax = high), size = lwd) +
              geom_rect(aes(xmin = date - width / 2 * 0.9, xmax = date + width/2 * 0.9,
                            ymin = pmin(open, close), ymax = pmax(open, close), fill = chg)) +
              guides(fill = FALSE, colour = FALSE) +
              scale_fill_manual(values = c("dn" = "tomato", "up" = "royalblue1")) +
              ggtitle(chart.title) +
              labs(x = x.title, y = y.title) + {
              if (is.null(x.interval)) scale_x_date(date_labels = date.format, expand = c(.01, 0)) else {
                scale_x_date(breaks = data[date.seq, "date"], labels = scales::date_format(date.format), expand = c(.01, .01))} } + {
              if (dc.lines %in% c("both", "low")) geom_path(aes(y = dc$low), colour = "gray24") } + {
              if (dc.lines %in% c("both", "high")) geom_path(aes(y = dc$high), colour = "gray24") } +
              plot_theme(...)

  # Handle special case of drawing a flat bar where OHLC = Open:
  if (any(data$flat_bar, na.rm = T)) {
    g <- g + geom_segment(data = data[data$flat_bar, ],
                          aes(x = date - width / 2 * 0.9,
                              y = close,
                              yend = close,
                              xend = date + width / 2 * 0.9))
  }

  return(g)
}
