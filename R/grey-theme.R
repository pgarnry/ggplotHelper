#' Function for designing a beautiful grey theme for ggplot2
#' @param legend.position character string specifying the legend position
#' (see details)
#' @details The ggplot2 package comes with four options for setting the
#' legend position - "left", "right", "bottom" and "top"
#' @export

grey_theme <- function(legend.position = "bottom") {

  # Generate the colors for the chart procedurally with RColorBrewer
  palette <- brewer.pal("Greys", n = 9)
  color.background = palette[2]
  color.grid.major = palette[3]
  color.axis.text = palette[6]
  color.axis.title = palette[7]
  color.title = palette[9]

  # Begin construction of chart
  theme_bw()+#base_size = 12) +

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
    theme(legend.text = element_text(size = 14, color = color.axis.title)) +
    theme(legend.title = element_blank()) +
    theme(legend.margin = unit(0, "cm")) +

    # Set title and axis labels, and format these and tick marks
    theme(plot.title = element_text(color = color.title, size = 20, vjust = 2.5)) +
    theme(axis.text.x = element_text(size = 14, color = color.axis.text)) +
    theme(axis.text.y = element_text(size = 14, color = color.axis.text)) +
    theme(axis.title.x = element_text(size = 15, color = color.axis.title, vjust = 0.5)) +
    theme(axis.title.y = element_text(size = 15, color = color.axis.title, vjust = 1.5)) +

    # Plot margins
    theme(plot.margin = unit(c(1, 1, 0.5, 0.7), "cm"))

}
