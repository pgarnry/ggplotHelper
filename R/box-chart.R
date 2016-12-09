#' Create box chart
#'
#' This function creates an overlay plotting a bar chart in ggplot
#' @param data data frame containing data for plotting
#' @param y character string specifying column name in data that should be y variable
#' @param x character string specifying column name in data that should be x variable
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param flip logical indicating whether box chart should be flipped or not
#' @param legend.names character string specifying legend names (default is NULL)
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param box.colours logical indicating whether boxes should have same colours or not
#' @param min.lim numeric setting minimum limit on y-axis
#' @param max.lim numeric setting maximum limit on y-axis
#' @details
#' Legend names can be modified through a character vector with same length as
#' number of categorical variables in the x variable. If not set, legend names
#' will take its names from the x variable.
#' @examples
#' box_chart(mtcars, "mpg", "cyl", title = "Miles per gallon across cylinders",
#' sub.title = "(Period 2000-2005)", y.title = "Miles per gallon",
#' x.title = "Cylinders", legend.position = "none")
#' @export

box_chart <- function(data, y, x = NULL, title = NULL, sub.title = NULL,
                      flip = FALSE, legend.names = NULL, y.title = NULL,
                      x.title = NULL, box.colours = FALSE,
                      min.lim = NULL, max.lim = NULL, ...) {

  # stop if input object is not a data.frame
  if (!is.data.frame(data)) stop("Input object has to be data.frame")

  # stop if variable name in data frame for values not set
  if (is.null(y)) stop("y should correspond to a variable name in input data frame")

  # define grouping (as factor) and insert space in the end for pretty legend labels
  if (is.null(x)) {
    x <- as.factor(0)
    palette <- chart_colours()[1]
  } else {
    data[, x] <- as.factor(data[, x])
    if (box.colours){
      palette <- chart_colours()[1:nlevels(data[, x])]
    } else {
      palette <- rep(chart_colours()[1], nlevels(data[, x]))
    }
    if (is.null(legend.names)) {
      legend.names <- paste(" ", as.character(levels(data[, x])), "   ")
    }
  }

  # if x and y axis titles are not NULL include line break
  if (!is.null(x.title)) x.title <- paste("\n", x.title)
  if (!is.null(y.title)) y.title <- paste(y.title, "\n")

  # define chart title object
  if (is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  }

  # create ggplot
  g <- ggplot(data, aes_string(y = y, x = x, fill = x), environment = environment()) +
    geom_boxplot(alpha = .7, size = .7, outlier.colour = "#d7191c", fatten = 1.2) +
    scale_fill_manual(values = palette,
                      labels = legend.names) +
    scale_y_continuous(limits = c(min.lim, max.lim)) +
    ggtitle(chart.title) +
    labs(x = x.title, y = y.title) +
    plot_theme(...) + {
      if (x == 0) theme(axis.text.x = element_blank())
    } + {
      if (flip) coord_flip()
    }

  return(g)

}
