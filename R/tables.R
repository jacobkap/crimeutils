#' Create a descriptive statistics table from numeric variables
#'
#' @param data
#' A data.frame with the data you want to make the table from.
#' @param columns
#' A string or vector of strings with the names of the columns you want to use.
#' @param output
#' A string or vector of strings indicating which math functions you want to
#' perform on the columns and present in the table. Options are: 'min',
#' 'median', 'mean', 'sd', 'max', and 'N'. Default is to use all of these
#' math functions. The order you put in these values is the order
#' the table will present the columns.
#' @param decimals
#' A positive integer for how many decimal places you want to round to.
#' @param title
#' A string with the text you want as the title
#' @param subtitle
#' A string with the text you want as the subtitle.
#' @param footnote
#' A string with the text you want as the footnote.
#'
#' @return
#' A data.frame with the data that generates the table, which is outputted
#' in the Viewer tab.
#' @export
#'
#' @examples
#' make_desc_stats_table(mtcars, columns = c("mpg", "disp", "wt", "cyl"))
#'
#' make_desc_stats_table(mtcars, c("mpg", "disp", "wt"), output = c("mean", "min"),
#' decimals = 4, title = "hello", subtitle = "world")
make_desc_stats_table <- function(data,
                                  columns,
                                  output = c("min", "median", "mean", "sd", "max", "sum", "NAs"),
                                  decimals = 2,
                                  title = NULL,
                                  subtitle = NULL,
                                  footnote = NULL) {


  if (!is.numeric(decimals) || decimals < 0) {
    stop("decimals must be a positive integer")
  }
  decimals <- round(decimals)
  if (!all(output %in% c("min", "median", "mean", "sd", "max", "N", "sum", "NAs"))) {
    stop("Please choose one or more of the following values for the output parameter: 'min', 'median', 'mean', 'sd', 'max', 'N', 'sum', or 'NAs'.")
  }


  summarize_types <- list("min"    = ~ min(.x, na.rm = TRUE),
                          "median" = ~ median(.x, na.rm = TRUE),
                          "mean"   = ~ mean(.x, na.rm = TRUE),
                          "sd"     = ~ sd(.x, na.rm = TRUE),
                          "max"    = ~ max(.x, na.rm = TRUE),
                          "N"      = length,
                          "sum"    = ~ sum(.x, na.rm = TRUE),
                          "NAs"    = ~ sum(is.na(.x)))
  # Keeps only output values the user wants.
  summarize_types <- summarize_types[names(summarize_types) %in% output]
  # Put the values in the order as inputted (if not, follows the default)
  summarize_types = summarize_types[output]
  names(summarize_types) <- paste0("zzz", names(summarize_types))
  summarize_types_no_z <- gsub("zzz", "", summarize_types)

  temp <-
    data %>%
    dplyr::select(dplyr::all_of(columns)) %>%
    dplyr::summarise_all(summarize_types) %>%
    dplyr::mutate_all(round, digits = decimals) %>%
    dplyr::mutate_all(prettyNum, format = "f", big.mark = ",")

  temp <- data.frame(t(temp))
  temp$variable <- rownames(temp)
  rownames(temp) <- 1:nrow(temp)
  temp$summarize_type <- gsub(".*_zzz", "", temp$variable)
  temp$variable <- gsub("_zzz.*", "", temp$variable)

  temp <- tidyr::spread(temp, key = "summarize_type", value = "t.temp.")
  # Reorder columns and rows
  temp <- temp[, order(c("variable", output), decreasing = TRUE)]
  temp <- temp[order(match(temp$variable, columns)), ]
  temp$variable <- capitalize_words(temp$variable)
  names(temp) <- capitalize_words(names(temp))
  names(temp) <- gsub("Nas", "NAs", names(temp))
  names(temp) <- gsub("Sd", "Std. Dev.", names(temp))

  gt::gt(temp) %>%
    gt::fmt_number(dplyr::one_of(summarize_types_no_z),
                   decimals = decimals) %>%
    gt::tab_header(title = title,
                   subtitle = subtitle) %>%
    gt::tab_source_note(source_note = footnote)

  return(temp)
}


#' Make a table showing the number (n) and percent of the population (e.g. \% of nrow())
#' for each value in a variable(s).
#'
#' @param data
#' A data.frame with the data you want to make the table from.
#' @param columns
#' A string or vector of strings with the column names to make the N and \% from.
#'
#' @return
#' A data.frame with one row for each value in the inputted variable(s) and columns
#' showing the N and \% for that value.
#' @export
#'
#' @examples
#' make_n_and_percent_table(mtcars, c("cyl", "gear"))
make_n_and_percent_table <- function(data, columns) {
  final <- data.frame()
  for (column in columns) {
    temp <- data %>%
      dplyr::group_by_at(column) %>%
      dplyr::summarise(n = dplyr::n(),
                       .groups = 'drop') %>%
      dplyr::mutate(freq = n / sum(n) * 100,
                    freq = crimeutils::pad_decimals(freq, 2),
                    n = prettyNum(n,big.mark = ","),
                    variable = "") %>%
      dplyr::select(variable,
                    dplyr::everything())

    if (is.factor(data[, column])) {
      temp <-
        temp %>%
        dplyr::arrange(column)
    } else {
      temp <-
        temp %>%
        dplyr::mutate(temp_n = readr::parse_number(n)) %>%
        dplyr::arrange(dplyr::desc(temp_n)) %>%
        dplyr::select(-temp_n)
    }

    names(temp)[2] <- "Description"
    temp$variable[1] <- column
    final <- final %>% dplyr::bind_rows(temp)
  }

  final$variable <- crimeutils::capitalize_words(final$variable)
  names(final) <- c("Variable", "Description", "N", "Percent")
  final <- data.frame(final)
  return(final)
}


#' Get mean and standard deviation of variables by group
#'
#' @param data
#' A data.frame with the data you want to make the table from.
#' @param group_column
#' A string with the name of the variable you are grouping by
#' @param columns
#' A string or vector of strings for the variables you want to get the mean
#' and standard deviation for.
#' @param total_row
#' A boolean (default TRUE) for whether to include a row a the bottom for the
#' overall mean and standard deviation (i.e. not by group).
#'
#' @return
#' A data.frame with the first column showing the category grouped by. Then
#' one column for each variable you want the mean and standard deviation for.
#' Will give the mean and standard deviation as a single string with the
#' standard deviation in parentheses.
#' @export
#'
#' @examples
#'make_mean_std_dev_by_group_table(mtcars, "gear", c("mpg", "disp"))
make_mean_std_dev_by_group_table <- function(data,
                                             group_column,
                                             columns,
                                             total_row = TRUE) {
  final <- data.frame()
  for (col in columns) {
    temp <- data %>%
      dplyr::group_by_at(group_column) %>%
      dplyr::summarize(mean = mean(get(col)),
                       sd       = stats::sd(get(col)),
                       .groups  = 'drop') %>%
      dplyr::mutate_if(is.numeric, round, 2) %>%
      dplyr::mutate_if(is.numeric, crimeutils::pad_decimals, 2) %>%
      dplyr::mutate(temp = paste0(mean, " (", sd, ")")) %>%
      dplyr::select(-mean,
                    -sd)
    names(temp)[2] <- col

    if (total_row) {
      data$dummy <- 1
      total <- data %>%
        dplyr::group_by(dummy) %>%
        dplyr::summarize(mean     = mean(get(col)),
                         sd       = stats::sd(get(col)),
                         .groups  = 'drop') %>%
        dplyr::mutate_if(is.numeric, round, 2) %>%
        dplyr::mutate_if(is.numeric, crimeutils::pad_decimals, 2) %>%
        dplyr::mutate(temp = paste0(mean, " (", sd, ")")) %>%
        dplyr::select(-mean,
                      -sd)
      names(total) <- c(group_column, col)
      total[1, 1] <- "Total"
      temp <- dplyr::bind_rows(temp, total)
    }

    if (nrow(final) == 0) {
      final <- temp
    } else {
      final <- dplyr::left_join(final, temp, by = group_column)
    }
  }
  final <- data.frame(final)
  return(final)
}
