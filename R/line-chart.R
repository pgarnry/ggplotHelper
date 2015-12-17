#' Create line chart
#'
#' This function is a convinient overlay for creating a beautiful line
#' plot using ggplot2
#' @param df data frame containing data for plotting
#' @param y specifying column name in df that should be y variable
#' @param x specifying column name in df that should be x variable
#' @param flip logical indicating whether barchart should be flipped or not
#' @param title character string specifying chart title
#' @param y.title character string specifying y-axis title
#' @param x.title character string specifying x-axis title
#' @details
#' The ellipsis is used to pass on arguments to the grey_theme function. Primary
#' use is to specify the legend.position to either "left", "right", "bottom" and "top".
#' @export

line_chart <- function(df, y, x = NULL, group = NULL, ...) {

  if(is.null(x)) {
    df$x <- 1:nrow(df)
    x <- "x"
  }

  g <- ggplot(df, aes_string(x = x, y = y, group = group), environment = environment()) +
    geom_line() +
    grey_theme()

}
