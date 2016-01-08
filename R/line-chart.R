#' Create line chart
#'
#' This function is a convinient overlay for creating a beautiful line
#' plot using ggplot2
#' @param df data frame containing data for plotting
#' @param y specifying column name in df that should be y variable
#' @param x specifying column name in df that should be x variable
#' @param title character string specifying chart title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param vline numeric specifying position of vertical line
#' @param hline numeric specifying position of horizontal line
#' @details
#' The ellipsis is used to pass on arguments to the grey_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom" and "top".
#' @export

line_chart <- function(df, y, x, group = NULL, title = NULL,
                       x.title = NULL, y.title = NULL, vline = NULL,
                       hline = NULL, x.interval = NULL, date.format = NULL,
                       min.lim = NULL, max.lim = NULL, ...) {

  # stop if input object is not a data.frame
  if(!is.data.frame(df)) stop("Input object has to be data.frame")

  # stop if grouping variable is not a factor
  if(!is.factor(df[, group]) & !is.null(group)) stop("Grouping variable should be a factor")

  # set colours based on grouping
  if(is.null(group)) {
    palette <- chart_colours()[1]
    id <- as.factor(y)
  } else {
    palette <- chart_colours()[1:nlevels(df[, group])]
    id <- as.factor(paste(" ", df[, group], "   "))
    df[, group] <- as.factor(paste(" ", df[, group], "   "))
  }

  # if x and y axis titles are not NULL include line break
  if(!is.null(x.title)) x.title <- paste("\n", x.title)
  if(!is.null(y.title)) y.title <- paste(y.title, "\n")

  # checks if x.interval is numeric and date.format is character if not NULL
  if(!is.null(x.interval) & !is.numeric(x.interval)) stop("x.interval should be numeric")
  if(!is.null(date.format) & !is.character(date.format)) stop("date.format should be character")

  # if NULL then it automatically sets minimum limits on x axis
  if(is.null(min.lim)) {
    min.lim <- floor(min(df[, y]) * .95)
  } else {
    min.lim <- min.lim
  }

  # if NULL then it automatically sets maximum limits on x axis
  if(is.null(max.lim)) {
    max.lim <- ceiling(max(df[, y]) * 1.05)
  } else {
    max.lim <- max.lim
  }

  # define accepted date-time classes, chron and timeDate classes are not accepted
  dt.classes <- c("POSIXct", "Date", "yearmon", "yearqtr")

  # check whether x variable is a recognized time-based class
  if(any(class(df[, x]) %in% dt.classes)) {

    if(dt.classes[1] %in% class(df[, x])) {

      # generate line chart with date class POSIXct
      g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) +
                  geom_line(aes(colour = id), size = 1.2) +
                  ggtitle(paste(title, "\n")) +
                  labs(x = x.title, y = y.title) + {
                    if(is.null(x.scale)) scale_x_datetime(date_labels = format("%Y-%m-%d %H:%M:%S")) else {
                      scale_x_datetime(date_breaks = x.scale[1], date_labels = x.scale[2], expand = c(.01, .5))}
                  } +
                  scale_y_continuous(expand = c(.01, 0)) +
                  grey_theme(...)

    }

    if(class(df[, x]) == "yearmon") {

      if(is.null(date.format)) date.format <- "%b %Y"

      if(!is.null(x.interval)) date.seq <- seq(1, length(df[,x]), x.interval)

      # generate line chart with date class yearmon
      g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) +
                  geom_line(aes(colour = id), size = 1.2) +
                  ggtitle(paste(title, "\n")) +
                  labs(x = x.title, y = y.title) + {
                    if(is.null(x.interval)) scale_x_yearmon(expand = c(.01, 0)) else {
                      scale_x_yearmon(breaks = df[date.seq, x], labels = scales::date_format(date.format), expand = c(.01, .01))}
                  } +
                  scale_y_continuous(limits=c(min.lim, max.lim), expand = c(.01, 0)) +
                  grey_theme(...)

    }

  } else {

    # generate simple line chart
    g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) + {
                if(!is.null(vline)) geom_vline(xintercept = vline, color = "#fe9929", size = 1)
                } + {
                if(!is.null(hline)) geom_hline(yintercept = hline, color = "#fe9929", size = 1)
                } +
                geom_line(aes(colour = id), size = 1.2) +
                scale_colour_manual(name = id, values = palette) +
                ggtitle(paste(title, "\n")) +
                labs(x = x.title, y = y.title) +
                scale_x_continuous(expand = c(.01, 0)) +
                scale_y_continuous(expand = c(.01, 0)) +
                grey_theme(...)

  }

  return(g)

}
