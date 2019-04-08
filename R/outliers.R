#' Title
#'
#' @param data
#' @param select_numeric_variables
#' @param group_variable
#' @param outlier_std_dev_value
#' @param zero_is_outlier
#'
#' @return
#' @export
#'
#' @examples
indicate_outliers <- function(data,
                              select_numeric_variables = NULL,
                              group_variable,
                              outlier_std_dev_value = 1.96,
                              zero_is_outlier = FALSE) {

  numeric_variables <- names(data)
  numeric_variables <- numeric_variables[sapply(data, is.numeric)]

  if (!is.null(select_numeric_variables)) {
    numeric_variables <- select_numeric_variables[select_numeric_variables %in% numeric_variables]
  }

  unique_groups <- unique(data[[group_variable]])
  for (numeric_variable in numeric_variables) {

    data[, paste0(numeric_variable, "_outlier", outlier_std_dev_value)]      <- 0
    data[, paste0(numeric_variable, "_outlier", outlier_std_dev_value, "_ever")] <- 0

    for (i in 1:length(unique_groups)) {

      current_group <- unique_groups[i]
      temp <-
        data %>%
        dplyr::filter(!!as.name(group_variable) %in% current_group)

      mean_value <- mean(temp[, numeric_variable], na.rm = TRUE)
      lower_outlier <- mean_value - (outlier_std_dev_value * sd(temp[, numeric_variable], na.rm = TRUE))
      upper_outlier <- mean_value + (outlier_std_dev_value * sd(temp[, numeric_variable], na.rm = TRUE))

      temp[, paste0(numeric_variable, "_outlier", outlier_std_dev_value)][temp[[numeric_variable]] < lower_outlier |
                                                                            temp[[numeric_variable]] > upper_outlier ] <- 1

      if (zero_is_outlier) {
        temp[, paste0(numeric_variable, "_outlier", outlier_std_dev_value)][temp[[numeric_variable]] %in% 0] <- 1
      }

      data[data[, group_variable] %in% current_group, paste0(numeric_variable, "_outlier", outlier_std_dev_value)] <-
        temp[, paste0(numeric_variable, "_outlier", outlier_std_dev_value)]

      if (any(temp[, paste0(numeric_variable, "_outlier", outlier_std_dev_value)] == 1)) {
        data[data[, group_variable] %in% current_group, paste0(numeric_variable, "_outlier",
                                                               outlier_std_dev_value, "_ever")] <- 1

      }
    }
  }
  return(data)
}
