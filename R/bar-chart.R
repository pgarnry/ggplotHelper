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
#' @param y.lim numeric vector with length two providing y-axis limits (min and max)
#' @param y.breaks numeric vector specifying where ticks appear
#' @details
#' The option bar.colour.name allows for multiple bars based on x-axis names to get a different colour.
#' If set to NULL all bars will have same default colour. Since ggplot2 version 2.2.0 this has changed
#' as the ordering gets complicated when y values contain both positive and negative values. As a result
#' bar colouring is only allowed decreasing is set to TRUE.
#'
#' @examples
#' data(mtcars)
#' mtcars$name <- rownames(mtcars)
#' bar_chart(mtcars, "mpg", "name", flip = TRUE,
#' title = "Miles per gallon across different car models",
#' scale.y = c(0, 40, 5),
#' decreasing = TRUE)
#' @export

bar_chart <- function(data, y, x, na.rm = FALSE, title = NULL,
                      sub.title = NULL, flip = FALSE, y.title = NULL,
                      x.title = NULL, decreasing = NULL, bar.colour.name = NULL,
                      y.lim = NULL, y.breaks = NULL, bar.width = NULL, ...) {

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

  # if x and y axis titles are not NULL include line break
  if(!is.null(x.title)) x.title <- paste("\n", x.title)
  if(!is.null(y.title)) y.title <- paste(y.title, "\n")

  # define chart title object
  if (!is.null(title) & !is.null(sub.title)) {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  } else if (!is.null(title) & is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- NULL
  }

  # setting colours
  palette <- rep(chart_colours()[1], nrow(data))

  # changing the ordering of y values
  if (!is.null(decreasing)) {
    data[, x] <- as.factor(data[, x])
    if (decreasing) {
      data[, x] <- reorder(data[, x], data[, y])
      if (!is.null(bar.colour.name)) {
        palette[match(bar.colour.name, levels(data[, x]))] <- "#f4a582"
      }
    }
    if (!decreasing) {
      data[, x] <- reorder(data[, x], -data[, y])
    }
  }

  # set values for breaks on y-scale if not set in option
  if (is.null(y.breaks)) {
    y.breaks <- waiver()
  }

  # creating chart
  g <- ggplot(data, aes_string(y = y, x = x), environment = environment()) +
              geom_bar(stat = "identity", fill = palette, width = bar.width) +
              ggtitle(chart.title) +
              labs(x = x.title, y = y.title) +
              plot_theme(...) +
              scale_y_continuous(limits = y.lim, breaks = y.breaks, expand = c(0.01, 0)) + {
              if (flip) coord_flip()} + {
              if (flip) theme(panel.grid.major.y = element_blank()) else theme(panel.grid.major.x = element_blank())}

  return(g)
}
