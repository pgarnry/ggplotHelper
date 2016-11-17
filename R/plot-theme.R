#' Function for designing a beautiful theme for ggplot2
#' @param legend.position character string specifying the legend position (see details)
#' @param base.size integer. Sets base font size (see details)
#' @param plot.margin numeric vector. Length should be four (see details)
#' @param color.theme character string specifying the (pre-defined) color theme - either "white" or "grey"
#' @details The ggplot2 package comes with four options for setting the
#' legend position - "left", "right", "bottom" and "top".
#' @import ggplot2
#' @export

plot_theme <- function(legend.position = "bottom", base.size = 10,
                       plot.margin = c(0.7, 1.2, 0.5, 0.5), color.theme = "white",
                       aspect.ratio = 1.61) {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- RColorBrewer::brewer.pal("Greys", n = 9)
  if (color.theme == "grey") {
    color.background = palette[2]
  } else if (color.theme == "white") {
    color.background = palette[1]
  } else stop ("color.theme only takes two values: grey or white")

  color.grid.major = palette[3]
  if (color.theme == "grey") {
    color.axis.text = palette[6]
    color.axis.title = palette[7]
  } else {
    color.axis.text = palette[8]
    color.axis.title = palette[8]
  }
  color.title = palette[9]

  # Begin construction of chart
  theme_bw() +

  theme(text = element_text(size = base.size)) +

  # Set the entire chart region to a light gray color
  theme(panel.background = element_rect(fill = color.background, color = color.background)) +
  theme(plot.background = element_rect(fill = color.background, color = color.background)) +
  theme(panel.border = element_rect(color = color.background)) +

  # Format the grid
  theme(panel.grid.major = element_line(color = color.grid.major, size = .5)) +
  theme(panel.grid.minor = element_blank()) +
  theme(axis.ticks = element_blank()) +

  # Format the legend, but hide by default
  theme(legend.position = legend.position) +
  theme(legend.background = element_rect(fill = color.background)) +
  theme(legend.text = element_text(size = rel(1.2), color = color.axis.title)) +
  theme(legend.title = element_blank()) +
  theme(legend.key = element_blank()) +
  theme(legend.margin = grid::unit(0.3, "cm")) +

  # Set title and axis labels, and format these and tick marks
  theme(plot.title    = element_text(color = color.title, size = rel(1.6), vjust = 0.5, hjust = 0.5)) +
  theme(axis.text.x   = element_text(size = rel(1), color = color.axis.text)) +
  theme(axis.text.y   = element_text(size = rel(1), color = color.axis.text)) +
  theme(axis.title.x  = element_text(size = rel(1.2), color = color.axis.title, hjust = 0.5)) +
  theme(axis.title.y  = element_text(size = rel(1.2), color = color.axis.title, vjust = 0.5)) +

  # Plot margins
  theme(plot.margin = grid::unit(plot.margin, "cm")) +

  # Format aspect ratio
  theme(aspect.ratio = aspect.ratio ^ -1)

}
