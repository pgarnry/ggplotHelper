#' Create line chart
#'
#' This function is a convinient overlay for creating a beautiful line
#' plot using ggplot2
#' @param df data frame containing data for plotting
#' @param y specifying column name in df that should be y variable
#' @param x specifying column name in df that should be x variable
#' @param group specifies the label for splitting data into multiple lines
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param vline numeric specifying position of vertical line
#' @param hline numeric specifying position of horizontal line
#' @param x.interval numeric used to set breaks when scaling the x variable
#' @param date.format character vector specifying the date label (see details)
#' @param min.lim numeric setting the minimum value on y-axis
#' @param max.lim numeric setting the maximum value on y-axis
#' @param lower.ribbon character string specifying column with values for lower ribbon
#' @param upper.ribbon character string specifying column with values for upper ribbon
#' @param legend.names character vector setting custom legend names
#' @param ribbon.names character string specifying custom legend name for ribbon
#' @details
#' Format codes for time series classes are defined in strftime. If date.format is
#' set to NULL the date format will be set to the most used format for each time
#' series class.
#'
#' The ellipsis is used to pass on arguments to the grey_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom", "top" or "none".
#' @export

line_chart <- function(df, y, x, group = NULL, title = NULL, sub.title = NULL,
                       x.title = NULL, y.title = NULL, vline = NULL,
                       hline = NULL, x.interval = NULL, date.format = NULL,
                       min.lim = NULL, max.lim = NULL, lower.ribbon = NULL,
                       upper.ribbon = NULL, legend.names = NULL,
                       ribbon.names = NULL, ...) {

  # stop if input object is not a data.frame
  if (!is.data.frame(df)) stop("Input object has to be data.frame")

  # define accepted date-time classes, chron and timeDate classes are not accepted
  dt.classes <- c("POSIXct", "Date", "yearmon", "yearqtr")

  if(!is.numeric(df[, x]) & all(class(df[, x]) != dt.classes)) {
    stop("x variable class should either be numeric or one of the time series classes
         POSIXct, Date, yearmon or yearqtr")
  }

  # define grouping, colouring and legend names
  if(is.null(group)) {
    group <- as.factor(x)
    line.colours <- chart_colours()[1]
    if(is.null(legend.names)) {
      legend.names <- paste(" ", x)
    }
  } else {
    df[, group] <- as.factor(df[, group])
    line.colours <- chart_colours()[1:nlevels(df[, group])]
    if(is.null(legend.names)) {
      legend.names <- paste(" ", as.character(levels(df[, group])), "   ")
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
  if (!is.null(x.interval) & !is.numeric(x.interval)) stop("x.interval should be numeric")
  if (!is.null(date.format) & !is.character(date.format)) stop("date.format should be character")

  # if NULL then it automatically sets minimum limits on y axis
  if (is.null(min.lim)) min.lim <- floor(min(df[, y]) * .95)

  # if NULL then it automatically sets maximum limits on y axis
  if (is.null(max.lim)) max.lim <- ceiling(max(df[, y]) * 1.05)

  # if lower.ribbon or upper.ribbon is not NULL
  if (!is.null(lower.ribbon) & !is.null(upper.ribbon)) {
    ribbon <- TRUE
  } else {
    ribbon <- FALSE
  }

  # check whether x variable is a recognized time-based class
  if (any(class(df[, x]) %in% dt.classes)) {

    if (class(df[, x]) == "POSIXct") {

      # generate line chart with date class POSIXct
      g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) +
                  geom_line(size = 1.2) +
                  scale_colour_manual(values = palette) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) + {
                    if (is.null(x.scale)) scale_x_datetime(date_labels = format("%Y-%m-%d %H:%M:%S")) else {
                       scale_x_datetime(date_breaks = x.scale[1], date_labels = x.scale[2], expand = c(.01, .5))}
                  } +
                  scale_y_continuous(expand = c(.01, 0)) +
                  grey_theme(...)

    }

    if (class(df[, x]) == "yearmon") {

      if (is.null(date.format)) date.format <- "%b %Y"

      if (!is.null(x.interval)) date.seq <- seq(1, length(df[,x]), x.interval)

      # generate line chart with date class yearmon
      g <- ggplot(df, aes_string(x = x, y = y, group = group, ymin = lower.ribbon, ymax = upper.ribbon), environment = environment()) +
                  geom_line(aes(colour = palette), size = 1.2) +
                  scale_colour_manual(values = palette,
                                      labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits=c(min.lim, max.lim), expand = c(.01, 0)) +
                  grey_theme(...) + {
                    if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)
                  } + {
                    if (ribbon) scale_fill_manual(values = chart_colours()[1],
                                                  labels = ribbon.names)
                  } + {
                    if (is.null(x.interval)) scale_x_yearmon(format = date.format, expand = c(.01, 0)) else {
                      scale_x_yearmon(breaks = df[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}
                  } + {
                    if (ribbon) theme(legend.box = "horizontal")
                  }

    }

    if (class(df[, x]) == "yearqtr") {

      if (is.null(date.format)) date.format <- "%Y Q%q"

      if (!is.null(x.interval)) date.seq <- seq(1, length(df[,x]), x.interval)

      # generate line chart with date class yearqtr
      g <- ggplot(df, aes_string(x = x, y = y, group = group, ymin = lower.ribbon, ymax = upper.ribbon), environment = environment()) +
                  geom_line(aes(colour = palette), size = 1.2) +
                  scale_colour_manual(values = palette,
                                      labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits=c(min.lim, max.lim), expand = c(.01, 0)) +
                  grey_theme(...) + {
                    if (ribbon) geom_ribbon(aes(fill = chart_colours()[1]), alpha = .2)
                  } + {
                    if (ribbon) scale_fill_manual(values = chart_colours()[1],
                                                  labels = ribbon.names)
                  } + {
                    if (is.null(x.interval)) scale_x_yearqtr(format = date.format, expand = c(.01, 0)) else {
                      scale_x_yearqtr(breaks = df[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}
                  } + {
                    if (ribbon) theme(legend.box = "horizontal")
                  }

    }

    if (class(df[, x]) == "Date") {

      if (is.null(date.format)) date.format <- "%Y-%m-%d"

      if (!is.null(x.interval)) {
        if(is.null(group)) {
          date.seq <- seq(1, length(df[,x]), x.interval)
        } else {
          date.seq <- seq(1, length(split(df[, x], levels(df[, group]))[[1]]), x.interval)
        }
      }

      # generate line chart with date class Date
      g <- ggplot(df, aes_string(x = x, y = y, group = group, ymin = lower.ribbon, ymax = upper.ribbon), environment = environment()) +
                  geom_line(aes(colour = df[, group]), size = 1.2) +
                  scale_colour_manual(values = line.colours,
                                      labels = legend.names) +
                  ggtitle(chart.title) +
                  labs(x = x.title, y = y.title) +
                  scale_y_continuous(limits=c(min.lim, max.lim), expand = c(.01, 0)) +
                  grey_theme(...) + {
                  if (is.null(x.interval)) scale_x_date(date_labels = date.format, expand = c(.01, 0)) else {
                     scale_x_date(breaks = df[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}
                  }
    }

  } else {

    # generate simple line chart
    g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) +
                geom_line(aes(colour = id), size = 1.2) +
                scale_colour_manual(name = id, values = palette) +
                ggtitle(chart.title) +
                labs(x = x.title, y = y.title) +
                scale_x_continuous(expand = c(.01, 0)) +
                scale_y_continuous(expand = c(.01, 0)) +
                grey_theme(...) + {
                  if (!is.null(vline)) geom_vline(xintercept = vline, color = "#636363", size = 1)
                } + {
                  if (!is.null(hline)) geom_hline(yintercept = hline, color = "#636363", size = 1)
                }

  }

  return(g)

}
