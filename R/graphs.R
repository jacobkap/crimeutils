#' Create a PDF with one time-series graph for each group in the data.
#'
#' @param data
#' A data.frame
#' @param numeric_variable
#' A string with the name of the column with numeric data to graph.
#' @param time_variable
#' A string with the name of the column that contains the time variable.
#' @param group_variable
#' A string with the name of the column with the grouping variable.
#' @param outlier_std_dev_value
#' A number that indicates how many standard deviations from the group mean
#' an outlier is. Outliers will be colored orange in the data.
#' @param file_name
#' A string with the name of the PDF to be made with one page for each graph.
#'
#' @return
#' A PDF with one page per graph
#' @export
#'
#' @examples
time_series_data_graph <- function(data,
                                   numeric_variable,
                                   time_variable,
                                   group_variable,
                                   outlier_std_dev_value = 1.96,
                                   file_name) {
  if (!grepl(".pdf$", file_name)) {
    file_name <- paste0(file_name, ".pdf")
  }

  outlier_zero_checker_temp <- rep("normal", nrow(data))
  outlier_zero_checker_temp[data[[numeric_variable]] == 0] <- "zero value"
  data$outlier_zero_checker <- outlier_zero_checker_temp
  unique_groups <- unique(data[[group_variable]])


  cols <- c("normal"         = "#000000",
            "zero value"     = "#1b9e77",
            "outlier"        = "#d95f02")


  grDevices::pdf(file_name,  width = 13, height = 8, onefile = TRUE)
  for (i in 1:length(unique_groups)) {

    current_group <- unique_groups[i]

    plot1 <-
      data %>%
      dplyr::filter(!!as.name(group_variable) %in% current_group)



    mean_value <- mean(plot1[, numeric_variable], na.rm = TRUE)
    lower_outlier <- mean_value - (outlier_std_dev_value *
                                     stats::sd(plot1[, numeric_variable], na.rm = TRUE))
    upper_outlier <- mean_value + (outlier_std_dev_value *
                                     stats::sd(plot1[, numeric_variable], na.rm = TRUE))

    plot1$outlier_zero_checker[plot1[[numeric_variable]] < lower_outlier |
                                 plot1[[numeric_variable]] > upper_outlier ] <- "outlier"
    number_of_outliers <- sum(plot1$outlier_zero_checker ==  "outlier")
    number_of_zeros    <- sum(plot1[[numeric_variable]]  %in% 0)

    plot1 <-
      plot1 %>%
      ggplot2::ggplot(ggplot2::aes_string(x = time_variable,
                                          y = numeric_variable)) +
      ggplot2::geom_point(ggplot2::aes(color = plot1$outlier_zero_checker),
                          size = 2.5) +
      ggplot2::stat_smooth(method = 'lm', se = FALSE) +
      ggplot2::ggtitle(paste(current_group, numeric_variable),
                       subtitle = paste0("Outlier = mean +- ",
                                         outlier_std_dev_value, " * standard deviations\n",
                                         "Outliers: ", number_of_outliers,
                                         "\nZeros:    ", number_of_zeros)) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::dollar) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 16),
                     axis.title.x = ggplot2::element_text(size = 14),
                     axis.title.y = ggplot2::element_text(size = 14)) +
      ggplot2::geom_hline(yintercept = mean_value,
                          color = "black",
                          linetype = "solid",
                          size = 1.05) +
      ggplot2::geom_hline(yintercept = lower_outlier,
                          color = "#d95f02",
                          linetype = "dashed",
                          size = 1.1)  +
      ggplot2::geom_hline(yintercept = upper_outlier,
                          color = "#d95f02",
                          linetype = "dashed",
                          size = 1.1)

    gridExtra::grid.arrange(plot1)
  }
  grDevices::dev.off()
}



#' Create a PDF with one scatterplot for each group in the data.
#'
#' @inheritParams time_series_data_graph
#'
#' @param numeric_variable1
#' A string with the name of the first column with numeric data to graph.
#' @param numeric_variable2
#' A string with the name of the second column with numeric data to graph.
#'
#' @return
#' A PDF with one page per graph
#' @export
#'
#' @examples
scatterplot_data_graph <- function(data,
                                   numeric_variable1,
                                   numeric_variable2,
                                   group_variable,
                                   file_name) {
  if (!grepl(".pdf$", file_name)) {
    file_name <- paste0(file_name, ".pdf")
  }
  zero_checker_temp <- rep("non_zero_value", nrow(data))
  zero_checker_temp[data[[numeric_variable1]] == 0 |
                      data[[numeric_variable2]] == 0] <- "zero_value"
  data$zero_checker <- zero_checker_temp
  unique_groups <- unique(data[[group_variable]])

  cols <- c("non_zero_value" = "#000000", "zero_value" = "#1b9e77")

  grDevices::pdf(file_name, width = 13, height = 8, onefile = TRUE)
  for (i in 1:length(unique_groups)) {
    current_group <- unique_groups[i]


    plot1 <-
      data %>%
      dplyr::filter(!!as.name(group_variable) %in% current_group) %>%
      ggplot2::ggplot(ggplot2::aes_string(x = numeric_variable1,
                                          y = numeric_variable2)) +
      ggplot2::geom_point(ggplot2::aes(color = data$zero_checker), size = 2.5) +
      ggplot2::stat_smooth(method = 'lm', se = FALSE) +
      ggplot2::ggtitle(paste(current_group, numeric_variable1,
                             numeric_variable2)) +
      ggplot2::theme_minimal() +
      ggplot2::scale_y_continuous(labels = scales::dollar) +
      ggplot2::scale_x_continuous(labels = scales::dollar) +
      ggplot2::scale_color_manual(values = cols) +
      ggplot2::theme(legend.position = "none",
                     plot.title = ggplot2::element_text(size = 16),
                     axis.title.x = ggplot2::element_text(size = 14),
                     axis.title.y = ggplot2::element_text(size = 14))

    gridExtra::grid.arrange(plot1)
  }
  grDevices::dev.off()
}






