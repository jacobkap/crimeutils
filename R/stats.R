#' Make a graph of coefficient values and 95 percent confidence interval for regression.
#'
#'
#' @param model
#' A `lm` object made from making a model using `lm()`.
#' @param coefficients
#' A string or vector of strings with the coefficient names. Will then make the graph
#' only with those coefficients.
#'
#' @return
#' Outputs a `ggplot2` graph
#' @export
#'
#' @examples
#' make_regression_graph(model = lm(mpg ~ cyl + disp + hp + drat, data = mtcars))
#' make_regression_graph(model = lm(mpg ~ cyl + disp + hp + drat, data = mtcars),
#' coefficients = c("cyl", "disp"))
#' make_regression_graph(model = lm(mpg ~ cyl + disp, data = mtcars))
make_regression_graph <- function(model, coefficients = NULL) {
  if (!(any(methods::is(model) %in% "lm"))) {
    stop("Input 'model' most be a 'lm' type made using the lm() function.")
  }



  data <- make_regression_table(model)
  if (!is.null(coefficients) & !all(coefficients %in% data$variable)) {
    stop("coefficients must be a string or vector of strings of coefficients.")
  }
  # Drops the intercept
  data <- data[-1, ]
  if (!is.null(coefficients)) {
    data <- data[data$variable %in% coefficients, ]
  }


  data$Significance <- "Not Significant"
  data$Significance[data$p_value < 0.05 & data$p_value > 0.01] <- "p<0.05"
  data$Significance[data$p_value <= 0.01] <- "p<0.01"


  ggplot2::ggplot(data, ggplot2::aes_string(x = "variable" , y = "estimate", color = "Significance")) +
    ggplot2::geom_errorbar(ggplot2::aes_string(ymin = 'confidence_interval_lower_95' ,
                                               ymax = 'confidence_interval_upper_95'),
                           width=.15, size = 1.02) +
    ggplot2::geom_point(size = 2.7) +
    ggplot2::coord_flip() +
    ggplot2::ylab("Estimate with 95% Confidence Intervals") +
    ggplot2::xlab("Variable") +
    crimeutils::theme_crim() +
    ggplot2::theme(legend.title = ggplot2::element_blank()) +
    ggplot2::scale_color_manual(values = c("#1b9e77", "#d95f02", "#7570b3"))

}



#' Turns regression results in a data.frame for easy conversion to a table
#'
#' @param model
#' A `lm` object made from making a model using `lm()`.
#'
#' @param coefficients_only
#' If TRUE (default), returns only the coefficients,standard error, t-value,
#' p-value, and confidence intervals. Else also returns the r-squared,
#' the adjusted r-squared,f-stat, p-value for the f-stat, and the degrees
#' of freedom.
#'
#' @return
#' A data.frame with the regression results
#' @export
#'
#' @examples
#' make_regression_table(lm(mpg ~ cyl, data = mtcars))
#' make_regression_table(lm(mpg ~ cyl, data = mtcars), coefficients_only = FALSE)
make_regression_table <- function(model, coefficients_only = TRUE) {
  if (!(any(methods::is(model) %in% "lm"))) {
    stop("Input 'model' most be a 'lm' type made using the lm() function.")
  }



  coefficients <- summary(model)$coefficients
  coefficients <- data.frame(coefficients)
  coefficients$variable <- rownames(coefficients)
  coefficients <-
    coefficients %>%
    dplyr::rename(estimate        = "Estimate",
                  standard_error  = "Std..Error",
                  t_value         = "t.value",
                  p_value         = "Pr...t..") %>%
    dplyr::select("variable",
                  dplyr::everything())

  confidence_intervals <- stats::confint(model)
  coefficients$confidence_interval_lower_95 <- confidence_intervals[, 1]
  coefficients$confidence_interval_upper_95 <- confidence_intervals[, 2]
  rownames(coefficients) <- 1:nrow(coefficients)

  if (coefficients_only) {
    return(coefficients)
  }

  suppressWarnings(final <- data.frame(r_squared           = summary(model)$r.squared,
                                       adjusted_r_squared  = summary(model)$adj.r.squared,
                                       f_statistic         = summary(model)$fstatistic["value"],
                                       f_statistic_p_value =  stats::pf(summary(model)$fstatistic[1],
                                                                        summary(model)$fstatistic[2],
                                                                        summary(model)$fstatistic[3],
                                                                        lower.tail=FALSE),
                                       degrees_of_freedom  = summary(model)$fstatistic["dendf"]))

  final <- dplyr::bind_cols(coefficients, final)
  rownames(final) <- 1:nrow(final)
  return(final)
}


make_percentile <- function() {

}
