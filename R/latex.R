# z <- mtcars[1:6, 1:6]
# z$mpg <- rownames(z)
# names(z)[1] <- ""
# multi_column <- c(" " = 1, "column1" = 2, "column2" = 3)

#make_latex_tables("test", "title", "label", z, "caption", multi_column, "NOTE ")
#' Creates a .tex file with LaTeX code to create a table from an R data.frame.
#'
#' @param data
#' A data.frame or a list of data.frames. If a data.frame, the table is created
#' with the values in that data.frame. If a list of data.frames, the table
#' gets one panel for each data.frame. If the list is named, will use the names
#' to create panel labels.
#' @param file
#' A string with the name of the file to save the .tex as.
#' @param caption
#' (Optional) A string with the caption for the table (i.e. the table title).
#' @param label
#' (Optional) A string with the reference for the table - to be used when referencing
#' the table in the text. If NULL,
#' @param multi_column
#' (Optional) A named vector with the names being the names of the multi-column and the
#' values being the width of the multi-column.
#' @param footnote
#' (Optional) A string with text for the footnote of the table.
#' @param sideways
#' (Optional) If TRUE, will make a sideways table (useful for large tables), otherwise
#' (default) will make a normal table.
#'
#' @return
#' Nothing. It will create a .tex file in the current working directory.
#' @export
#'
#' @examples
#' \dontrun{}
make_latex_tables <- function(data,
                              file,
                              caption = "",
                              label = "",
                              multi_column = NULL,
                              footnote = "",
                              sideways = FALSE) {
  if (!grepl(".tex$", file)) {
    file <- paste0(file, ".tex")
  }
  sink(file)

  table_direction <- "table"
  if (sideways) table_direction <- "sidewaystable"


  writeLines("\\clearpage")
  writeLines(paste0("\\begin{", table_direction, "}[H]"))
  writeLines("\\centering")
  writeLines(paste0("\\caption{", caption, "}"))
  writeLines("\\begin{subtable}[t]{\\linewidth}")
  writeLines(paste0("\\label{", label, "}"))


  if (is.data.frame(data)) {
    make_latex_table_panel(data, NULL, multi_column)
  } else if (is.list(data) && !is.data.frame(data)) {
    for (i in 1:length(data)) {
      if (i == 1) {
        make_latex_table_panel(data[[i]], names(data)[i], multi_column)
      } else {
        make_latex_table_panel(data[[i]], names(data)[i], NULL)
      }
    }
  }

  # End table
  writeLines("\\vspace{-6mm}")
  writeLines(paste0("\\floatfoot{", footnote, "}"))

  writeLines("\\end{subtable}")
  writeLines(paste0("\\end{", table_direction, "}"))

  sink()

}

make_big_ci_brackets <- function(.data) {
  .data <- gsub("\\[", "\\\\big\\[", .data)
  .data <- gsub("\\]", "\\\\big\\]", .data)
  return(.data)
}

make_b_to_beta <- function(.data) {
  .data <- gsub("^B$", "$\\\\hat{\\\\\\beta}$", .data)
  .data <- gsub("^Se\\(B\\)$", "Se\\($\\\\hat{\\\\\\beta}$\\)", .data)
  return(.data)
}

fix_percent <- function(.data) {
  .data <- gsub("%", "\\\\%", .data)
  return(.data)
}


get_column_alignments <- function(data) {
  alignment <- paste0("l", rep("r", times = ncol(data) - 1), collapse = "")
  return(alignment)
}

make_latex_table_panel <- function(data, panel_caption, multi_column) {
  alignment <- get_column_alignments(data)
  writeLines(paste0("\\begin{tabular}{@{\\extracolsep{5pt}}",
                    alignment, "}"))

  if (!is.null(multi_column)) {
    for (i in 1:length(multi_column)) {
      multi_column_row <- paste0("\\multicolumn{",
                                 unname(multi_column)[i],
                                 "}{l}{\\textbf{",
                                 names(multi_column[i]),
                                 "}} &")
      if (i == length(multi_column)) {
        multi_column_row <- gsub("&$", "\\\\\\\\", multi_column_row)
      }
      writeLines(multi_column_row)
    }
  }

  writeLines("\\toprule")

  headers <- paste0("\\thead{", names(data), "} &", collapse = " ")
  headers <- gsub("&$", "\\\\\\\\", headers)

  writeLines(headers)
  writeLines("\\midrule")

  data[] <- sapply(data, as.character)
  for (i in 1:nrow(data)) {
    row_data <- unname(unlist(data[i, ]))
    row_data <- make_big_ci_brackets(row_data)
    row_data <- make_b_to_beta(row_data)
    row_data <- fix_percent(row_data)
    row_data <- paste0(row_data, " &", collapse = " ")
    row_data <- gsub("&$", "\\\\\\\\", row_data)
    writeLines(row_data)
  }
  writeLines("\\end{tabular}")
  writeLines("\\vspace{5pt}")
  writeLines(paste0("\\caption{\\textbf{", panel_caption, "}}"))
}

