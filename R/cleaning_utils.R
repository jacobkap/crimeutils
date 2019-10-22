#' Prepare .tex output from kableExtra for Overleaf
#'
#'# When kableExtra makes .tex files it adds lines to create a full .tex
# document (e.g. \start{document}, and adds packages). This removes all that
# stuff to it can be added to Overleaf.
#'
#' @param files
#' A string or vector of strings naming the .tex files to use.
#' @param all_tex
#' A binary variable (default is FALSE) where if TRUE applies code to every .tex
#' file in working directory. Overrides any value in `files`.
#'
#' @return
#' @export
#'
#' @examples
clean_tex_files <- function(files = "", all_tex = FALSE) {
  if (all_tex) {
    files = list.files(pattern = ".tex")
  }
  for (.file in files) {
    .tex <- read_lines(.file)
    .tex <- .tex[grep("begin\\{table\\}", .tex):grep("end\\{table\\}", .tex)]
    write_lines(.tex, path = .file)
  }
}

#' Capitalizes the first letter of every word
#'
#' @param word
#' A string with words you want capitalized
#'
#' @return
#' The origin string with the first letter of each word capitalized
#' @export
#'
#' @examples
simple_cap <- function(word) {
  word <- tolower(word)
  word <- gsub("_", " ", word)
  word <- strsplit(word, " ")[[1]]
  word <- paste(toupper(substring(word, 1,1)),
                substring(word, 2),
                sep = "",
                collapse = " ")
  return(word)
}
