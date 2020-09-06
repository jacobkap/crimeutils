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
    dplyr::mutate_all(prettyNum, format="f", big.mark=",")

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



  temp %>%
    gt::gt() %>%
    gt::fmt_number(dplyr::one_of(summarize_types_no_z),
                   decimals = decimals) %>%
    gt::tab_header(title = title,
                   subtitle = subtitle) %>%
    gt::tab_source_note(source_note = footnote)

  return(temp)
}
