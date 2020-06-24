
#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
 scale_linetype_crim <- function(...) {
  return(ggplot2::scale_linetype_manual(..., values = c("solid",
                                                        "dotted",
                                                        "longdash",
                                                        "twodash",
                                                        "dashed")))
}

#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
scale_fill_crim <- function(...) {
  return(ggplot2::scale_fill_manual(..., values = c('#990000',
                                                    '#011f5b',
                                                    '#8d0e8f',
                                                    '#000000',
                                                    "#ff7f00")))
}


#' Title
#'
#' @param ...
#'
#' @return
#' @export
#'
#' @examples
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


#' Title
#'
#' @return
#' @export
#'
#' @examples
theme_crim <- function() {
  ggplot2::theme(legend.position   = "bottom",
                 legend.text       = ggplot2::element_text(size = 15),
                 legend.title      = ggplot2::element_text(size = 14),
                 legend.box.margin = ggplot2::margin(-13, 0, 0, 0),
                 rect              = ggplot2::element_rect(colour = "black",
                                                           fill = "white"),
                 panel.background  = ggplot2::element_blank(),
                 panel.border      = ggplot2::element_blank(),
                 axis.line         = ggplot2::element_line(colour = "black",
                                                           size = ggplot2::rel(1)),
                 panel.grid.major  = ggplot2::element_line(colour = "grey"),
                 panel.grid.minor  = ggplot2::element_blank(),
                 text              = ggplot2::element_text(size = 15,
                                                           family = "serif"),
                 plot.background   = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 legend.background = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 legend.key        = ggplot2::element_rect(fill="white",
                                                           colour=NA),
                 plot.title        = ggplot2::element_text(size = 24,
                                                           hjust = 0.5,
                                                           face = "bold"),
                 axis.text         = ggplot2::element_text(size = 14,
                                                           color = "black"),
                 axis.title        = ggplot2::element_text(size = 18,
                                                           face = "bold"),
                 axis.title.x      = ggplot2::element_text(vjust =  1.1),
                 axis.title.y      = ggplot2::element_text(vjust = 1.5),
                 axis.ticks.length = ggplot2::unit(.13, "cm"))
}
