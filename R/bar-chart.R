#' Function for plotting beautiful barcharts in ggplot
#' @param data data frame containing data for plotting
#' @param y character string specifying column name in data that should be y variable
#' @param x character string specifying column name in data that should be x variable
#' @param na.rm a logical indicating whether NA values should be stripped
#' before the computation proceeds
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param flip logical indicating whether barchart should be flipped or not
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param decreasing logical indicating whether y values should be ordered and in which direction
#' @param bar.colour.name character string indicating which bars to be coloured based on x names (see details)
#' @param scale.y numeric vector with length three providing y-axis limits (min and max) and breaks (see details)
#' @details
#' The option bar.colour.name allows for multiple bars based on x-axis names to get a different colour.
#' If set to NULL all bars will have same default colour.
#'
#' The scale.y numeric vector has length three and indicates the y-axis limits and break points.
#' An input vector of c(0, 60, 10) would translate into y-axis with minimum value at 0 and
#' maximum value of 60 with breaks for every 10 between the minimum and maximum values.
#'
#' Otherwise see example for clarification on the scale.y variable.
#' @examples
#' data(mtcars)
#' mtcars$name <- rownames(mtcars)
#' bar_chart(mtcars, "mpg", "name", flip = TRUE,
#' title = "Miles per gallon across different car models",
#' scale.y = c(0, 40, 5))
#' @export

bar_chart <- function(data, y, x, na.rm = FALSE, title = NULL,
                      sub.title = NULL, flip = FALSE, y.title = NULL,
                      x.title = NULL, decreasing = NULL,
                      bar.colour.name = NULL, scale.y = NULL, ...) {

  # stop if input object is not a data.frame and x and y variables not specified
  if (!is.data.frame(data)) stop("Input object has to be data.frame")
  if (is.null(y)) stop("y should correspond to a variable name in input data frame")
  if (is.null(x)) stop("x should correspond to a variable name in input data frame")

  # remove rows in data.frame if NA values exist in y variable
  if (na.rm) {
    if(any(is.na(data[, y]))) data <- data[-which(is.na(data[, y])), ]
  }

  # stop if NA values exist in y varible
  if (any(is.na(data[, y]))) stop("NA values exist in y variable. Set na.rm = TRUE or remove NA values from data.frame manually")

  # define chart title object
  if (!is.null(title) & !is.null(sub.title)) {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  } else if (!is.null(title) & is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- NULL
  }

  # changing the ordering of y values
  if (!is.null(decreasing)) {
    if (decreasing) {
      data[, x] <- factor(data[, x], levels = data[order(data[, y], decreasing = F), x])
    }
    if (!decreasing) {
      data[, x] <- factor(data[, x], levels = data[order(data[, y], decreasing = T), x])
    }
  }

  # set values for scaling y
  if (!is.null(scale.y)) {
    y.limits <- scale.y[1:2]
    y.breaks <- seq(scale.y[1], scale.y[2], scale.y[3])
    scale.y <- TRUE
  } else {
    scale.y <- FALSE
  }

  # setting colours
  palette <- rep(chart_colours()[1], nrow(data))
  if (!is.null(bar.colour.name)) {
    palette[match(bar.colour.name, levels(data[, x]))] <- "#f4a582"
  }

  # creating chart
  g <- ggplot(data, aes_string(y = y, x = x), environment = environment()) +
              geom_bar(stat = "identity", fill = palette) +
              ggtitle(chart.title) +
              labs(x = x.title, y = y.title) +
              grey_theme() + {
                if (scale.y) scale_y_continuous(limits = y.limits, breaks = y.breaks)
              } + {
                if (flip) coord_flip()
              } + {
                if (flip) theme(panel.grid.major.y = element_blank()) else theme(panel.grid.major.x = element_blank())
              }

  return(g)

}
