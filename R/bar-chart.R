#' Function for plotting beautiful barcharts in ggplot
#' @param df data frame containing data for plotting
#' @param y character string specifying column name in df that should be y variable
#' @param x character string specifying column name in df that should be x variable
#' @param flip logical indicating whether barchart should be flipped or not
#' @param title character string specifying chart title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @examples
#' mtcars$name <- rownames(mtcars)
#' bar_chart(mtcars, "mpg", "name", flip = TRUE,
#' title = "Miles per gallon across different car models")
#' @export

bar_chart <- function(df, y, x, title = NULL, sub.title = NULL,
                      flip = FALSE, y.title = NULL, x.title = NULL, ...) {

  if(is.null(y)) stop("y should correspond to a variable name in input data frame")
  if(is.null(x)) stop("x should correspond to a variable name in input data frame")

  # define chart title object
  if (is.null(sub.title)) {
    chart.title <- paste(title, "\n")
  } else {
    chart.title <- bquote(atop(.(title, "\n"), atop(.(sub.title), "")))
  }

  g <- ggplot(df, aes_string(y = y, x = x), environment = environment()) +
    geom_bar(stat = "identity", fill = chart_colours()[1]) +
    grey_theme(...) +
    ggtitle(chart.title) +
    labs(x = x.title, y = y.title) + {
      if(flip) coord_flip()
    }

  return(g)

}
