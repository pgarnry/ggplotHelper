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
                       hline = NULL, ...) {

  # stop if input object is not a data.frame
  if(!is.data.frame(df)) stop("Input object has to be data.frame")

  # stop if grouping variable is not a factor
  if(!is.factor(df[, group])) stop("Grouping variable should be a factor")

  # set colours based on grouping
  if(is.null(group)) {
    palette <- chart_colours()[1]
  } else {
    palette <- chart_colours()[1:nlevels(df[, group])]
    df[, group] <- as.factor(paste(" ", df[, group], "   "))
  }

  # generate ggplot
  g <- ggplot(df, aes_string(x = x, y = y, colour = group), environment = environment()) + {
    if(!is.null(vline)) geom_vline(xintercept = vline, color = "#fe9929", size = 1)
    } + {
    if(!is.null(hline)) geom_hline(yintercept = hline, color = "#fe9929", size = 1)
    } +
    geom_line(size = 1.2) +
    scale_colour_manual(values = palette) +
    ggtitle(title) +
    labs(x = x.title, y = y.title) +
    scale_x_continuous(expand = c(.01, 0)) +
    scale_y_continuous(expand = c(.01, .05)) +
    grey_theme(...)

  return(g)

}
