#' Save ggplots into multiple png files
#'
#' This function takes ggplots and save them into png images with fixed size
#' @param numeric. Indicates the multiplication factor to the default width (777 pixels) and height (480 pixels)
#' @details The function saves png files with filename equal to the name of the ggplot object.
#' The png image is saved with a height of 480 pixels and width scaled by the golden ratio to 777
#' (776.66 but rounded to nearest integer)
#' @examples
#' g1 <- density_chart(mtcars, "mpg", "cyl")
#' mtcars$name <- rownames(mtcars)
#' g2 <- bar_chart(mtcars, "mpg", "name", flip = T)
#' save_png(g1,g2)
#' @export

save_png <- function(base.size = 1, ...) {

  # get names from input objects and use them to name list of plots
  snm <- sapply(substitute(list(...)), deparse)[-1]
  plots <- list(...)
  names(plots) <- snm

  # create png images
  for(i in 1:length(plots)) {
    png(file = paste(names(plots)[i], ".png", sep = ""),
        width = 777 * base.size, height = 480 * base.size)
    plot(plots[[i]])
    dev.off()
  }
}
