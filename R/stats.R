

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
  if (!(any(is(model) %in% "lm"))) {
    stop("Input 'model' most be a 'lm' type made using the lm() function.")
  }



  coefficients <- summary(model)$coefficients
  coefficients <- data.frame(coefficients)
  coefficients$variable <- rownames(coefficients)
  coefficients <-
    coefficients %>%
    dplyr::rename(estimate = Estimate,
                  standard_error  = Std..Error,
                  t_value         = t.value,
                  p_value         = Pr...t..) %>%
    dplyr::select(variable,
                  dplyr::everything())

  confidence_intervals <- confint(model)
  coefficients$confidence_interval_lower_95 <- confidence_intervals[, 1]
  coefficients$confidence_interval_upper_95 <- confidence_intervals[, 2]
  rownames(coefficients) <- 1:nrow(coefficients)

  if (coefficients_only) {
    return(coefficients)
  }

  suppressWarnings(final <- data.frame(r_squared           = summary(model)$r.squared,
                                       adjusted_r_squared  = summary(model)$adj.r.squared,
                                       f_statistic         = summary(model)$fstatistic["value"],
                                       f_statistic_p_value =  pf(summary(model)$fstatistic[1],
                                                                 summary(model)$fstatistic[2],
                                                                 summary(model)$fstatistic[3],
                                                                 lower.tail=FALSE),
                                       degrees_of_freedom  = summary(model)$fstatistic["dendf"]))

  final <- dplyr::bind_cols(coefficients, final)
  rownames(final) <- 1:nrow(final)
  return(final)
}
