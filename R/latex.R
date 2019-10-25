# z <- mtcars[1:6, 1:6]
# z$mpg <- rownames(z)
# names(z)[1] <- ""
# multi_column <- c(" " = 1, "column1" = 2, "column2" = 3)

#make_latex_tables("test", "title", "label", z, "caption", multi_column, "NOTE ")
#' Creates a .tex file with LaTeX code to create a table from an R data.frame.
#'
#' @param file_name
#' @param table_title
#' @param table_reference_label
#' @param data
#' @param panel_caption
#' @param multi_column
#' @param footnote
#' @param sideways
#'
#' @return
#' @export
#'
#' @examples
make_latex_tables <- function(file_name,
                              table_title,
                              table_reference_label,
                              data,
                              panel_caption,
                              multi_column = NULL,
                              footnote = "",
                              sideways = FALSE) {
  if (!grepl(".tex$", file_name)) {
    file_name <- paste0(file_name, ".tex")
  }
  sink(file_name)

  table_direction <- "table"
  if (sideways) table_direction <- "sidewaystable"


  writeLines("\\clearpage")
  writeLines(paste0("\\begin{", table_direction, "}[H]"))
  writeLines("\\centering")
  writeLines(paste0("\\caption{", table_title, "}"))
  writeLines("\\begin{subtable}[t]{\\linewidth}")
  writeLines(paste0("\\label{", table_reference_label, "}"))


  if (is.data.frame(data)) {
    make_latex_table_panel(data, panel_caption, multi_column)
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
  .data <- gsub("^B$", "$\\\\hat{\\\\\beta}$", .data)
  .data <- gsub("^Se\\(B\\)$", "Se\\($\\\\hat{\\\\beta}$\\)", .data)
  return(.data)
}

fix_percent <- function(.data) {
  .data <- gsub("%", "\\\\%", .data)
  return(.data)
}


get_column_alignments <- function(data) {
  alignment <- paste0(rep("r", times = ncol(data)), collapse = "")
  return(alignment)
}

make_latex_table_panel <- function(data, panel_caption, multi_column) {
  alignment <- get_column_alignments(data)
  writeLines(paste0("\\begin{tabular}{{\\textwidth}@{\\extracolsep{5pt}}",
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

