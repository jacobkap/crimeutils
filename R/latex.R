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
#' @param longtable
#' (Optional) If TRUE, will make a longtable table (useful for long tables), otherwise
#' (default) will make a normal table.
#'
#' @return
#' Nothing. It will create a .tex file in the current working directory.
#' @export
#'
#' @examples
#' \dontrun{
#' make_latex_tables(mtcars, file =  "text.tex", caption = "This is a description of the table",
#' label = "internal_table_label", footnote = "Here is some info you should know to read this table",
#' longtable = TRUE)
#' }
make_latex_tables <- function(data,
                              file,
                              caption = "",
                              label = "",
                              multi_column = NULL,
                              footnote = "",
                              sideways = FALSE,
                              longtable = FALSE) {
  if (!grepl(".tex$", file)) {
    file <- paste0(file, ".tex")
  }
  sink(file)

  table_direction <- "table"
  if (sideways)  {
    table_direction <- "sidewaystable"
    message("Please include '\\usepackage{rotating}' at the start of your LaTeX file for sideways table to work.")
  }
  if (longtable)  {
    writeLines("\\begin{footnotesize}")
    table_direction <- "longtable"
    message("Please include '\\usepackage{longtable}' at the start of your LaTeX file for long table to work.")
  }


  writeLines(paste0("\\begin{", table_direction, "}"))
  if (longtable) {
    if (caption != "") {
      caption <- fix_percent(caption)
      writeLines(paste0("\\caption{", caption, "}"))
      caption <- ""
    }
  }
  if (!longtable) {
    writeLines("\\centering")
    writeLines("\\begin{subtable}[c]{1.1\\linewidth}")
  }


  if (is.data.frame(data)) {
    make_latex_table_panel(data, NULL, multi_column, longtable)
  } else if (is.list(data) && !is.data.frame(data)) {
    for (i in 1:length(data)) {
      if (i == 1) {
        make_latex_table_panel(data[[i]], names(data)[i], multi_column, longtable)
      } else {
        make_latex_table_panel(data[[i]], names(data)[i], NULL, longtable)
      }
    }
  }

  # End table
 # writeLines("\\bottomrule")
  if (footnote != "") {
    writeLines(paste0("\\floatfoot{", footnote, "}"))
  }
  if (!longtable) {
    writeLines("\\end{subtable}")
  }
  if (caption != "") {
    caption <- fix_percent(caption)
    writeLines(paste0("\\caption{", caption, "}"))
  }
  writeLines(paste0("\\label{table:", label, "}"))
  writeLines(paste0("\\end{", table_direction, "}"))
  if (longtable) {
    writeLines("\\end{footnotesize}")
  }
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
  .data <- gsub("\\$", "\\\\$", .data)
  .data <- gsub("#", "\\\\#", .data)
  return(.data)
}


get_column_alignments <- function(data) {

  alignment <- "l"
  for (i in 2:ncol(data)) {
    col_values <- stringr::str_extract_all(data[, i, drop = TRUE], stringr::boundary("character"))
    col_values <- unlist(col_values)
    col_values <- unique(col_values)
    if (all(col_values %in% c(0:9, "*", "[", "]", "-", ",", ".", "~", " "))) {
      alignment <- paste0(alignment, "r", collapse = "")
    } else {
      alignment <- paste0(alignment, "l", collapse = "")
    }
  }
  return(alignment)
}

make_latex_table_panel <- function(data, panel_caption, multi_column, longtable) {
  alignment <- get_column_alignments(data)
  if (!longtable) {
    writeLines(paste0("\\begin{tabular}{@{\\extracolsep{4pt}}",
                      alignment, "}"))
  }

  if (!is.null(multi_column)) {
    for (i in 1:length(multi_column)) {
      multi_column_row <- paste0("\\multicolumn{",
                                 unname(multi_column)[i],
                                 "}{r}{\\textbf{",
                                 names(multi_column[i]),
                                 "}} &")
      # Replace first {r} to {l} so it aligns to the left for the very first column,
      # This this column is usually the category of the row so tends to be left-aligned.
      multi_column_row <- sub("\\{r\\}", "\\{l\\}", multi_column_row)
      if (i == length(multi_column)) {
        multi_column_row <- gsub("&$", "\\\\\\\\", multi_column_row)
      }
      writeLines(multi_column_row)
    }
  }

  if (!longtable) {
    writeLines("\\toprule")
  }

  col_name <- names(data)[1]
  col_name <- gsub("([A-z]{9}.?) ([A-z])", "\\1 \\\\\\\\ \\2", col_name)
  if (longtable) {
    headers <- paste0("& ", col_name, " & ", collapse = " ")
  } else {
    headers <- paste0(" \\thead[l]{", col_name, "} &", collapse = " ")
  }

  for (i in 2:ncol(data)) {
    col_name <- names(data)[i]
    col_name <- gsub("([A-z]{9}.?) ([A-z])", "\\1 \\\\\\\\ \\2", col_name)
    col_values <- stringr::str_extract_all(data[, i, drop = TRUE], stringr::boundary("character"))
    col_values <- unlist(col_values)
    col_values <- unique(col_values)
    if (longtable) {
      headers <- paste0(headers, col_name, " &", collapse = " ")
    }  else if (all(col_values %in% c(0:9, "*", "[", "]", "-", ",", ".", "~", " "))) {
      headers <-  paste0(headers, " \\thead[r]{", col_name, "} &", collapse = " ")
    } else {
      headers <-  paste0(headers, " \\thead[l]{", col_name, "} &", collapse = " ")
    }
  }
  headers <- fix_percent(headers)
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
  if (!longtable) {
    writeLines("\\bottomrule")
    writeLines("\\end{tabular}")
  }
  writeLines("\\vspace{5pt}")
  if (!is.null(panel_caption)) {
    writeLines(paste0("\\caption{\\textbf{", fix_percent(panel_caption), "}}"))
  }
}

