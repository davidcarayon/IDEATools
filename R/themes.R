#' IDEA ggplot2 theme
#'
#' @param base_size base size
#' @param base_family base family
#' @import ggplot2
#'
#' @return a ggplot2 theme for IDEA
#' @importFrom ggplot2 theme_grey theme element_line element_rect element_text margin element_blank unit rel
theme_idea <- function(base_size = 15, base_family = "") {
  # colors
  blue <- "#2c3e50"
  green <- "#18BC9C"
  white <- "#FFFFFF"
  grey <- "grey80"

  # Starts with theme_grey and then modify some parts
  ggplot2::theme_grey(base_size = base_size, base_family = base_family) %+replace%

    ggplot2::theme(

      # Base Inherited Elements
      line = ggplot2::element_line(
        colour = blue, size = 0.5, linetype = 1,
        lineend = "butt"
      ),
      rect = ggplot2::element_rect(
        fill = white, colour = blue,
        size = 0.5, linetype = 1
      ),
      text = ggplot2::element_text(
        family = base_family, face = "plain",
        colour = blue, size = base_size,
        lineheight = 0.9, hjust = 0.5, vjust = 0.5, angle = 0,
        margin = ggplot2::margin(), debug = FALSE
      ),

      # Axes
      axis.line = ggplot2::element_blank(),
      axis.text = ggplot2::element_text(size = rel(0.8)),
      axis.ticks = ggplot2::element_line(color = grey, size = rel(1 / 3)),
      axis.title = ggplot2::element_text(size = rel(1.0), face = "bold"),

      # Panel
      panel.background = ggplot2::element_rect(fill = white, color = NA),
      panel.border = ggplot2::element_rect(fill = NA, size = rel(1 / 2), color = blue),
      panel.grid.major = ggplot2::element_line(color = grey, size = rel(1 / 3)),
      panel.grid.minor = ggplot2::element_line(color = grey, size = rel(1 / 3)),
      panel.grid.minor.x = ggplot2::element_blank(),
      panel.spacing = ggplot2::unit(.75, "cm"),

      # Legend
      legend.key = ggplot2::element_rect(fill = white, color = NA),
      legend.position = "top",
      legend.title = element_text(face = "bold"),

      # Strip (Used with multiple panels)
      strip.background = ggplot2::element_rect(fill = "#2c3e50", color = "black"),
      strip.text = ggplot2::element_text(color = white, face = "bold", size = ggplot2::rel(0.7), margin = ggplot2::margin(t = 5, b = 5)),

      # Plot
      plot.title = ggplot2::element_text(
        size = rel(1.2), hjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 4, l = 0, unit = "pt")
      ),
      plot.subtitle = ggplot2::element_text(
        size = rel(0.9), hjust = 0,
        margin = ggplot2::margin(t = 0, r = 0, b = 3, l = 0, unit = "pt")
      ),

      # Complete theme
      complete = TRUE
    )
}
