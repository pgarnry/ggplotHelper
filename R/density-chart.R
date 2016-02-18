#' Create density chart
#'
#' This function is a convinient overlay for creating a beautiful density
#' plot using ggplot2
#' @param df data frame containing data for plotting
#' @param x character string specifying name of x variable in data frame
#' @param group character string for grouping of x
#' @param title character string specifying chart title
#' @param sub.title character string specifying chart sub title
#' @param legend.names character string specifying legend names
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param transparency numeric setting the level of colour transparency
#' @param min.lim numeric setting minimum limit on x-axis
#' @param max.lim numeric setting maximum limit on x-axis
#' @param vline logical value for drawing vertical lines for the distribution median of x
#' @param vline.custom numeric setting position on vertical line not tied to the distribution of x
#' @details
#' Group variable not ideally be a factor but the function will force class factor on the variable.
#'
#' Use legend names variable to set custom labels on legends. If legend names is NULL the function
#' will force legend names to the colnames of either x or names of groups depending on the input.
#'
#' The ellipsis is used to pass on arguments to the grey_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom" and "top".
#'
#' Vline option specifies whether vertical lines showing the median of the distribution
#' should be plotted.
#' @examples
#' density_chart(mtcars, "mpg", "cyl", title = "Miles per gallon",
#' sub.title = "(per groups of cylinders)", vline = TRUE)
#' @export

density_chart <- function(df, x, group = NULL, title = NULL, sub.title = NULL,
                          legend.names = NULL, y.title = NULL, x.title = NULL,
                          transparency = .3, min.lim = NULL, max.lim = NULL,
                          vline = FALSE, vline.custom = NULL, ...) {

  # stop if input object is not a data.frame
  if(!is.data.frame(df)) stop("Input object has to be data.frame")

  # stop if variable name in data frame for values not set
  if(is.null(x)) stop("x should correspond to a variable name in input data frame")

  # define grouping, colouring and legend names
  if(is.null(group)) {
    group <- as.factor(x)
    palette <- chart_colours()[1]
    if(is.null(legend.names)) {
      legend.names <- paste(" ", x)
    }
    if(vline) {
      vline.df <- data.frame(median = median(df[, x]))
    }
  } else {
    df[, group] <- as.factor(df[, group])
    palette <- chart_colours()[1:nlevels(df[, group])]
    if(is.null(legend.names)) {
      legend.names <- paste(" ", as.character(levels(df[, group])), "   ")
    }
    if(vline) {
      vline.df <- aggregate(df[, x], list(df[, group]), median)
      colnames(vline.df) <- c("group", "median")
    }
  }

  # if x and y axis titles are not NULL include line break
  if(!is.null(x.title)) x.title <- paste("\n", x.title)
  if(!is.null(y.title)) y.title <- paste(y.title, "\n")

  # define chart title object
  if(is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  }

  # if NULL then it automatically sets minimum limits on x axis
  if(is.null(min.lim)) min.lim <- floor(mean(df[, x]) - 3 * sd(df[, x]))

  # if NULL then it automatically sets maximum limits on x axis
  if(is.null(max.lim)) max.lim <- ceiling(mean(df[, x]) + 3 * sd(df[, x]))

  g <- ggplot(df, aes_string(x = x, fill = group), environment = environment()) +
    geom_density(alpha = transparency, linetype = 0) +
    scale_fill_manual(values = palette,
                      labels = legend.names) +
    scale_x_continuous(limits=c(min.lim, max.lim)) + {
      if(vline) geom_vline(data = vline.df, aes(xintercept = median), color = palette, linetype = "dashed", size = 1)
    } + {
      if(is.numeric(vline.custom)) geom_vline(xintercept = vline.custom, color = "#636363", size = 1)
    } +
    ggtitle(chart.title) +
    labs(x = x.title, y = y.title) +
    scale_y_continuous(expand = c(.001, 0)) +
    grey_theme(...)

  return(g)

}
