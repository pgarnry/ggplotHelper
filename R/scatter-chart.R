#' Create scatter chart
#'
#' This function is a convinient overlay for creating a beautiful scatter
#' plot using ggplot2
#' @param data data frame containing data for plotting
#' @param x character string specifying name of x variable in data frame
#' @param y character
#' @param na.rm a logical indicating whether NA values should be stripped
#' before the computation proceeds
#' @param x.names character string specifying column name of names for
#' textual annotations
#' @param x.names.show numeric indicating how many point names should be shown (details)
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param transparency numeric setting the level of colour transparency
#' @param x.min numeric setting minimum limit on x-axis
#' @param x.max numeric setting maximum limit on x-axis
#' @param y.min numeric setting minimum limit on y-axis
#' @param y.max numeric setting maximum limit on y-axis
#' @details
#' The x.names.show is based on distance between actual and fitted values.
#' If x.names.show is set to 3 then the three observations with the largest
#' absolute distance between actual and fitted values will have their name
#' shown next to the point.
#' @examples
#' density_chart(mtcars, "mpg", "cyl", title = "Miles per gallon",
#' sub.title = "(per groups of cylinders)", vline = TRUE)
#' @export

scatter_chart <- function(data, x, y, na.rm = FALSE, x.names = NULL, x.names.show = NULL,
                          title = NULL, sub.title = NULL, y.title = NULL, x.title = NULL,
                          x.min = NULL, x.max = NULL, y.min = NULL, y.max = NULL, ...) {

  # stop if input object is not a data.frame and x and y variables are not defined
  if(!is.data.frame(data)) stop("Input object has to be data.frame")
  if(is.null(x)) stop("x should correspond to a variable name in input data frame")
  if(is.null(y)) stop("y should correspond to a variable name in input data frame")

  # stop if NA values exist in y varible
  if (any(is.na(data[, y]))) stop("NA values exist in y variable. Set na.rm = TRUE or remove NA values from data.frame manually")

  # remove rows in data.frame if NA values exist in y variable
  if (na.rm) {
    data <- data[-which(is.na(data[, y])), ]
  }

  # define chart colours
  point.colour <- chart_colours()[1]
  line.colour <- "#236291"
  ribbon.colour <- "#6ba2cb"

  # define legend name for prediction interval
  ribbon.legend <- "  1 sd (prediction interval)"

  # if x and y axis titles are not NULL include line break
  if(!is.null(x.title)) x.title <- paste("\n", x.title)
  if(!is.null(y.title)) y.title <- paste(y.title, "\n")

  # define chart title object
  if(is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  }

  # calculate interquartile range
  iqr.y <- (quantile(data[, y], prob = .75) - quantile(data[, y], prob = .25)) * 3
  iqr.x <- (quantile(data[, x], prob = .75) - quantile(data[, x], prob = .25)) * 3

  # select y and x observations based on interquartile range
  sel.y <- which(data[, y] < mean(data[, y]) + iqr.y & data[, y] > mean(data[, y]) - iqr.y)
  sel.x <- which(data[, x] < mean(data[, x]) + iqr.x & data[, x] > mean(data[, x]) - iqr.x)

  # adjust input data frame with selected observations above
  data.adj <- data[intersect(sel.x, sel.y), ]

  # fit a linear regression on trimmed data
  fit <- lm(paste(y, "~", x), data.adj)

  # calculate prediction interval
  pred <- predict(fit, newdata = data, interval = "prediction", level = 0.683)

  # insert fit, lower and upper bound into input data frame
  data$fit <- pred[, "fit"]
  data$lwr <- pred[, "lwr"]
  data$upr <- pred[, "upr"]

  fit.spread <- abs(data[, y] - data$fit)

  # define point.names
  if(!is.null(x.names)) {
    if(!is.character(data[, x.names])) stop("x.names should have class character")
    point.names <- data[, x.names]
    point.names[-rev(order(fit.spread))[1:x.names.show]] <- ""
  } else {
    point.names <- rep("", nrow(data))
  }

  # if NULL then it automatically sets minimum and maximum limits on x axis
  if(is.null(x.min)) x.min <- floor(min(data[, x]))
  if(is.null(x.max)) x.max <- ceiling(max(data[, x]))

  # if NULL then it automatically sets maximum limits on x axis
  if(is.null(y.min)) y.min <- floor(min(data$lwr))
  if(is.null(y.max)) y.max <- ceiling(max(data$upr))

  # plot scatter chart with regression and prediction interval
  g <- ggplot(data, aes_string(x = x, y = y, ymin = "lwr", ymax = "upr"), environment = environment()) +
              geom_point(shape = 19, cex = 3, colour = point.colour) +
              geom_text(aes(label = point.names), hjust = -0.15, vjust = 0.5, colour = point.colour, size = 5) +
              geom_line(data = data, aes_string(y = "fit", x = x), colour = line.colour, size = 1) +
              geom_ribbon(aes(fill = ribbon.colour), alpha = .2) +
              scale_fill_manual(values = ribbon.colour,
                                labels = ribbon.legend) +
              scale_x_continuous(limits = c(x.min, x.max)) +
              scale_y_continuous(limits = c(y.min, y.max)) +
              ggtitle(chart.title) +
              labs(x = x.title, y = y.title) +
              grey_theme(...)

  return(g)

}
