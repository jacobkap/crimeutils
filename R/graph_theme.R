
#' A set of linetypes
#'
#' @param ...
#' Arguments passed to discrete_scale()
#'
#' @return
#' The ggplot graph with linetypes set.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, linetype = as.character(cyl))) +
#'   ggplot2::geom_line(size = 1) +
#'   scale_linetype_crim() +
#'   theme_crim()
scale_linetype_crim <- function(...) {
  return(ggplot2::scale_linetype_manual(..., values = c(1, 3, 5, 2, 4, 6)))

}

#' A set of colorblind friendly fill colors for graphs.
#'
#' @param ...
#' Arguments passed to discrete_scale()
#'
#' @return
#' The ggplot graph with fills set.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = cyl, fill = as.character(cyl))) +
#'  ggplot2::geom_bar() +
#'   scale_fill_crim()

scale_fill_crim <- function(...) {
  return(ggplot2::scale_fill_manual(..., values = c('#990000',
                                                    '#011f5b',
                                                    '#8d0e8f',
                                                    '#000000',
                                                    "#ff7f00")))
}


#' A set of colorblind friendly colors for graphs.
#'
#' @param ...
#' Arguments passed to discrete_scale()
#'
#' @return
#' The ggplot graph with colors set.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars, ggplot2::aes(x = mpg, y = hp, color = as.character(cyl))) +
#'   ggplot2::geom_point(size = 2) +
#'   scale_color_crim()
scale_color_crim <- function(...) {
  return(ggplot2::scale_color_manual(..., values = c('#990000',
                                                     '#011f5b',
                                                     '#8d0e8f',
                                                     '#000000',
                                                     "#ff7f00")))
}


# ggplot(mtcars, aes(x = mpg, y = hp, color = as.character(cyl))) +
#   geom_line() +
#   ggtitle("Title") +
#   xlab("X-axis Label") +
#   ylab("Y-axis Label") +
#   labs(colour="Legend Title") +
#   theme_crim() +
#   scale_color_crim() +
#   scale_x_continuous(expand = c(0, 0))


#' A minimalist theme designed for graphics in academic research
#'
#' @return
#' The graph with the theme changed.
#' @export
#'
#' @examples
#' ggplot2::ggplot(mtcars) +
#' ggplot2::geom_point(ggplot2::aes(x = wt, y = mpg)) +
#' theme_crim()
theme_crim <- function() {
  ggplot2::theme(legend.position   = "bottom",
                 legend.text       = ggplot2::element_text(size = 16),
                 legend.title      = ggplot2::element_text(size = 18),
                 legend.box.margin = ggplot2::margin(-13, 0, 0, 0),
                 rect              = ggplot2::element_rect(colour = "black",
                                                           fill = "white"),
                 panel.background  = ggplot2::element_blank(),
                 panel.border      = ggplot2::element_blank(),
                 axis.line         = ggplot2::element_line(colour = "black",
                                                           size = ggplot2::rel(1)),
                 panel.grid.major  = ggplot2::element_line(colour = "grey"),
                 panel.grid.minor  = ggplot2::element_line(colour = "grey"),
                 text              = ggplot2::element_text(size = 15,
                                                           family = "serif"),
                 plot.background   = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 legend.background = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 legend.key        = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 legend.key.size   = ggplot2::unit(1, "cm"),
                 plot.title        = ggplot2::element_text(size = 24,
                                                           hjust = 0.5,
                                                           face = "bold"),
                 axis.text         = ggplot2::element_text(size = 16,
                                                           color = "black"),
                 axis.title        = ggplot2::element_text(size = 22,
                                                           face = "bold"),
                 axis.title.x      = ggplot2::element_text(vjust =  1.5),
                 axis.title.y      = ggplot2::element_text(vjust = 1.7),
                 axis.ticks.length = ggplot2::unit(.13, "cm"))
}
