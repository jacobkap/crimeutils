#' Creates new columns to indicate which values are outliers based on the average value.
#'
#' @param data
#' A data.frame
#' @param select_columns
#' A string or vector of strings with the name(s) of the numeric columns to check
#' for outliers. If NULL (default), will use all numeric columns in the data.
#' @param group_variable
#' A string with the name of the column with the grouping variable.
#' @param std_dev_value
#' A number indicating how many standard deviations away from the mean to
#' determine if a value is an outlier.
#' @param zero_is_outlier
#' If TRUE (not default), reports any zero value as an outlier.
#'
#' @return
#' The initial data.frame with new columns for each numeric variable included with a value of 0 if not an outlier and 1 if that row is an outlier.
#' @export
#'
#' @examples
indicate_outliers <- function(data,
                              select_columns = NULL,
                              group_variable,
                              std_dev_value = 1.96,
                              zero_is_outlier = FALSE) {

  numeric_cols <- names(data)
  numeric_cols <- numeric_cols[sapply(data, is.numeric)]

  if (!is.null(select_columns)) {
    numeric_cols <- select_columns[select_columns %in% numeric_cols]
  }

  unique_groups <- unique(data[[group_variable]])
  for (numeric_col in numeric_cols) {

    data[, paste0(numeric_col, "_outlier",
                  std_dev_value)] <- 0
    data[, paste0(numeric_col, "_outlier",
                  std_dev_value, "_ever")] <- 0
    data[, paste0(numeric_col, "_outlier",
                  std_dev_value, "_freq")] <- 0

    for (i in 1:length(unique_groups)) {

      current_group <- unique_groups[i]
      temp <-
        data %>%
        dplyr::filter(!!as.name(group_variable) %in% current_group)

      mean_value <- mean(temp[, numeric_col], na.rm = TRUE)
      lower_outlier <- mean_value - (std_dev_value * stats::sd(temp[, numeric_col], na.rm = TRUE))
      upper_outlier <- mean_value + (std_dev_value * stats::sd(temp[, numeric_col], na.rm = TRUE))

      temp[, paste0(numeric_col, "_outlier",
                    std_dev_value)][temp[[numeric_col]] <
                                              lower_outlier |
                                              temp[[numeric_col]] >
                                              upper_outlier ] <- 1

      if (zero_is_outlier) {
        temp[, paste0(numeric_col, "_outlier",
                      std_dev_value)][temp[[numeric_col]] %in% 0] <- 1
      }

      data[data[, group_variable] %in% current_group,
           paste0(numeric_col,
                  "_outlier", std_dev_value)] <-
        temp[, paste0(numeric_col, "_outlier",
                      std_dev_value)]

      if (any(temp[, paste0(numeric_col, "_outlier",
                            std_dev_value)] == 1)) {
        data[data[, group_variable] %in% current_group,
             paste0(numeric_col, "_outlier",
                    std_dev_value, "_ever")] <- 1

      }
      # Number of times that group has had an outlier!!!!!!!
      data[data[, group_variable] %in% current_group,
           paste0(numeric_col, "_outlier",
                  std_dev_value, "_freq")] <-
        sum(temp[, paste0(numeric_col, "_outlier",
                          std_dev_value)])
    }
  }
  return(data)
}
