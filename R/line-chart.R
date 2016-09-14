#' Create line chart
#'
#' This function is a convinient overlay for creating a beautiful line
#' plot using ggplot2
#' @param data data frame containing data for plotting
#' @param y specifying column name in data that should be y variable
#' @param x specifying column name in data that should be x variable
#' @param group specifies the label for splitting data into multiple lines
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param vline numeric specifying position of vertical line
#' @param hline numeric specifying position of horizontal line
#' @param x.interval numeric used to set number of breaks when scaling the x variable
#' @param date.format character vector specifying the date label (see details)
#' @param y.min numeric setting the minimum value on y-axis
#' @param y.max numeric setting the maximum value on y-axis
#' @param ribbon.lwr character string specifying column with values for lower ribbon
#' @param ribbon.upr character string specifying column with values for upper ribbon
#' @param legend.names character vector setting custom legend names
#' @param ribbon.names character string specifying custom legend name for ribbon
#' @details
#' Format codes for time series classes are defined in strftime. If date.format is
#' set to NULL the date format will be set to the most used format for each time
#' series class.
#'
#' The ellipsis is used to pass on arguments to the plot_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom", "top" or "none".
#' Arguments can also be sent to plot_theme function to adjust plot margins - see function
#' for more details.
#' @export

line_chart <- function(data = NULL, y, x = NULL, group = NULL, title = NULL, sub.title = NULL,
                       x.title = NULL, y.title = NULL, vline = NULL, hline = NULL,
                       x.interval = NULL, date.format = NULL, y.min = NULL, y.max = NULL,
                       ribbon.lwr = NULL, ribbon.upr = NULL, legend.names = NULL,
                       ribbon.names = NULL, aspect.ratio = 1.61, ...) {

  # define line colour if only numeric vector is provided
  if (is.null(data) & is.null(x)) {
    if (!is.numeric(y))  stop ("If only y is specified then it has to be a numeric vector")
    if (!is.null(group)) stop ("If only y is specified no group is allowed")
    line.colours <- chart_colours()[1]
    if (is.null(y.min)) y.min <- min(y) - abs(min(y)) * .05
    if (is.null(y.max)) y.max <- max(y) + abs(max(y)) * .05
  } else {

    # if NULL then it automatically sets minimum and maximum limits on y axis
    if (is.null(y.min)) y.min <- min(data[, y]) - abs(min(data[, y])) * .05
    if (is.null(y.max)) y.max <- max(data[, y]) + abs(max(data[, y])) * .05

    # define grouping, colouring and legend names if data.frame is provided
    if (!is.data.frame(data)) stop ("Input object has to be data.frame when x is defined")
    if (is.null(group)) {
      line.colours <- chart_colours()[1]
      if (is.null(legend.names)) {
        legend.names <- paste(" ", y)
      }
    } else {
      data[, group] <- as.factor(data[, group])
      line.colours <- chart_colours()[1:nlevels(data[, group])]
      if (is.null(legend.names)) {
        legend.names <- paste(" ", as.character(levels(data[, group])), "   ")
      }
    }
  }

  # if x and y axis titles are not NULL include line break
  if (!is.null(x.title)) x.title <- paste("\n", x.title)
  if (!is.null(y.title)) y.title <- paste(y.title, "\n")

  # define chart title object
  if (!is.null(title) & !is.null(sub.title)) {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  } else if (!is.null(title) & is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else
    chart.title <- NULL

  # checks if x.interval is numeric and date.format is character if not NULL
  if (!is.null(x.interval)  & !is.numeric(x.interval))    stop("x.interval should be numeric")
  if (!is.null(date.format) & !is.character(date.format)) stop("date.format should be character")

  # if ribbon.lwr or ribbon.upr is not NULL
  if (!is.null(ribbon.lwr) & !is.null(ribbon.upr)) {
    ribbon <- TRUE
  } else {
    ribbon <- FALSE
  }

  # define accepted date-time classes, chron and timeDate classes are not accepted
  dt.classes <- c("POSIXct", "Date", "yearmon", "yearqtr")

  # check whether x variable is a recognized time-based class
  if (any(class(data[, x]) %in% dt.classes) & !is.null(x)) {

    # find the class of x variable
    class.x <- class(data[, x])

    if ("POSIXct" %in% class.x) {

      if (!is.null(x.interval)) time.seq <- seq(1, length(data[, x]), floor(x.interval / length(data[, x])))

      # generate line chart with date class POSIXct
      g <- ggplot(data, aes_string(x = x, y = y, group = group, ymin = ribbon.lwr, ymax = ribbon.upr), environment = environment()) + {
                  if (is.null(group)) geom_line(aes(colour = line.colours), size = 1.2) else {
                                      geom_line(aes(colour = data[, group]), size = 1.2)}} +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")} + {
                  if (is.null(x.interval)) scale_x_datetime(date_labels = format("%Y-%m-%d %H:%M:%S")) else {
                                           scale_x_datetime(breaks = data[time.seq, x], expand = c(.01, .5))}}

    }

    if (class.x == "yearmon") {

      if (is.null(date.format)) date.format <- "%b %Y"
      if (!is.null(x.interval)) date.seq <- seq(1, length(data[, x]), floor(x.interval / length(data[, x])))

      # generate line chart with date class yearmon
      g <- ggplot(data, aes_string(x = x, y = y, group = group, ymin = ribbon.lwr, ymax = ribbon.upr), environment = environment()) + {
                  if (is.null(group)) geom_line(aes(colour = line.colours), size = 1.2) else {
                                      geom_line(aes(colour = data[, group]), size = 1.2)}} +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")} + {
                  if (is.null(x.interval)) scale_x_yearmon(format = date.format, expand = c(.01, 0)) else {
                                           scale_x_yearmon(breaks = data[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}} +
                  theme(aspect.ratio = 1.61^-1)

    }

    if (class.x == "yearqtr") {

      if (is.null(date.format)) date.format <- "%Y Q%q"
      if (!is.null(x.interval)) date.seq <- seq(1, length(data[,x]), floor(x.interval / length(data[,x])))

      # generate line chart with date class yearqtr
      g <- ggplot(data, aes_string(x = x, y = y, group = group, ymin = ribbon.lwr, ymax = ribbon.upr), environment = environment()) + {
                  if (is.null(group)) geom_line(aes(colour = line.colours), size = 1.2) else {
                                      geom_line(aes(colour = data[, group]), size = 1.2)}} +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")} + {
                  if (is.null(x.interval)) scale_x_yearqtr(format = date.format, expand = c(.01, 0)) else {
                                           scale_x_yearqtr(breaks = data[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}}

    }

    if (class.x == "Date") {

      if (is.null(date.format)) date.format <- "%Y-%m-%d"

      if (!is.null(x.interval)) {
        if(is.null(group)) {
          date.seq <- seq(1, length(data[,x]), x.interval)
        } else {
          date.length <- length(split(data[, x], levels(data[, group]))[[1]])
          date.seq <- seq(1, date.length, floor(date.length / x.interval))
        }
      }

      # generate line chart with date class Date
      g <- ggplot(data, aes_string(x = x, y = y, group = group, ymin = ribbon.lwr, ymax = ribbon.upr), environment = environment()) + {
                  if(is.null(group)) geom_line(aes(colour = line.colours), size = 1.2) else {
                                     geom_line(aes(colour = data[, group]), size = 1.2)}} +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")} + {
                  if (is.null(x.interval)) scale_x_date(date_labels = date.format, expand = c(.01, 0)) else {
                                           scale_x_date(breaks = data[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}}

    }

  } else {

    if(is.null(data) & is.null(x)) {

      # set up data.frame object to use in ggplot
      data <- data.frame(x = 1:length(y),
                         y = y)

      # generate a simple line chart with no pre-defined x-variable
      g <- ggplot(data, aes(x = x, y = y)) +
                  geom_line(aes(colour = line.colours), size = 1.2) +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (!is.null(vline)) geom_vline(xintercept = vline, color = "#636363", size = 1)} + {
                  if (!is.null(hline)) geom_hline(yintercept = hline, color = "#636363", size = 1)} + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")}

    } else {

      # generate simple line chart if data is a data.frame with both y and x variable
      g <- ggplot(data, aes_string(x = x, y = y, group = group), environment = environment()) + {
                  if(is.null(group)) geom_line(aes(colour = line.colours), size = 1.2) else {
                                     geom_line(aes(colour = data[, group]), size = 1.2)}} +
                  scale_colour_manual(values = line.colours, labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits = c(y.min, y.max), expand = c(.01, 0)) +
                  plot_theme(...) + {
                  if (!is.null(vline)) geom_vline(xintercept = vline, color = "#636363", size = 1)} + {
                  if (!is.null(hline)) geom_hline(yintercept = hline, color = "#636363", size = 1)} + {
                  if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)} + {
                  if (ribbon) scale_fill_manual(values = chart_colours()[1], labels = ribbon.names)} + {
                  if (ribbon) theme(legend.box = "horizontal")}

    }

  }

  return(g)
}
