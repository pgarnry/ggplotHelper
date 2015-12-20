#' Create density chart
#'
#' This function is a convinient overlay for creating a beautiful density
#' plot using ggplot2
#' @param df data frame containing data for plotting
#' @param x character string specifying name of x variable in data frame
#' @param group character string for grouping of x
#' @param title character string specifying chart title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @param transparency numeric setting the level of colour transparency
#' @param min.lim numeric setting minimum limit on x-axis
#' @param max.lim numeric setting maximum limit on x-axis
#' @param vline logical value for drawing vertical lines for the distribution median of x
#' @param vline.custom numeric setting position on vertical line not tied to the distribution of x
#' @details
#' The ellipsis is used to pass on arguments to the grey_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom" and "top".
#'
#' Vline option specifies whether vertical lines showing the median of the distribution
#' should be plotted.
#' @examples
#' density_chart(mtcars,"mpg","cyl",title="Miles per gallon",vline=TRUE)
#' @export

density_chart <- function(df, x, group = NULL, title = NULL, y.title = NULL,
                          x.title = NULL, transparency = .3,
                          min.lim = NULL, max.lim = NULL, vline = FALSE,
                          vline.custom = NULL, ...) {

  # stop if input object is not a data.frame
  if(!is.data.frame(df)) stop("Input object has to be data.frame")

  # stop if variable name in data frame for values not set
  if(is.null(x)) stop("x should correspond to a variable name in input data frame")

  # define grouping (as factor) and insert space in the end for pretty legend labels
  if(!is.null(group)) {
    id <- as.factor(paste(" ", df[, group], "   "))
    df[, group] <- as.factor(paste(" ", df[, group], "   "))
    if(vline) {
      vline.df <- aggregate(df[, x], list(df[, group]), median)
      colnames(vline.df) <- c("group", "median")
    }
  } else {
    id <- as.factor(x)
    if(vline) {
      vline.df <- data.frame(median = median(df[, x]))
    }
  }

  # set colours based on grouping
  palette <- chart_colours()[1:nlevels(id)]

  # if NULL then it automatically sets minimum limits on x axis
  if(is.null(min.lim)) {
    min.lim <- floor(mean(df[, x]) - 3 * sd(df[, x]))
  } else {
    min.lim <- min.lim
  }

  # if NULL then it automatically sets maximum limits on x axis
  if(is.null(max.lim)) {
    max.lim <- ceiling(mean(df[, x]) + 3 * sd(df[, x]))
  } else {
    max.lim <- max.lim
  }

  g <- ggplot(df, aes_string(x = x, group = group), environment = environment()) +
    geom_density(aes(fill = id), alpha = transparency, linetype = 0) +
    scale_fill_manual(values = palette) +
    scale_x_continuous(limits=c(min.lim, max.lim)) + {
      if(vline) geom_vline(data = vline.df, aes(xintercept = median), color = palette, linetype = "dashed")
    } + {
      if(is.numeric(vline.custom)) geom_vline(xintercept = vline.custom, color = "#fe9929", size = 1)
    } +
    ggtitle(paste(title, "\n")) +
    labs(x = paste("\n", x.title), y = paste(y.title, "\n")) +
    scale_y_continuous(expand = c(.001, 0)) +
    grey_theme(...)

  return(g)

}
