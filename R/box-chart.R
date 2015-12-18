#' Create box chart
#'
#' This function creates an overlay plotting a bar chart in ggplot
#' @param df data frame containing data for plotting
#' @param x character string specifying column name in df that should be x variable
#' @param group character string setting the group variable in the data.frame
#' @param flip logical indicating whether box chart should be flipped or not
#' @param title character string specifying chart title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @export

box_chart <- function(df, x, group = NULL, flip = FALSE, title = NULL,
                      y.title = NULL, x.title = NULL, ...) {

  # stop if input object is not a data.frame
  if(!is.data.frame(df)) stop("Input object has to be data.frame")

  # stop if grouping variable is not a factor
  if(!is.factor(df[, group]) & !is.null(group)) stop("Grouping variable should be a factor")

  # set boxplot colour
  palette <- chart_colours()[1]

  # create ggplot
  g <- ggplot(df, aes_string(y = x, x = group), environment = environment()) +
    geom_boxplot(fill = palette, colour = "#404040", alpha = .7, size = .7, outlier.colour = "#d7191c") +
    ggtitle(title) +
    labs(x = x.title, y = y.title) +
    grey_theme(...) + {
      if(flip) coord_flip()
    }

  return(g)

}
