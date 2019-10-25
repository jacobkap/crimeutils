#' Pad decimal places with trailing zeros.
#'
#' @param numbers
#' A number or vector of numbers to pad.
#' @param digits
#' Number of decimal places to pad. If NULL (default), uses the maximum
#' number of decimal places in the numbers input.
#'
#' @return
#' The original numbers but with trailing zeros added to the decimal places.
#' @export
#'
#' @examples
pad_decimals <- function(numbers, digits = NULL) {
  numbers  <- as.character(numbers)
  decimals <- strsplit(numbers, "\\.")
  decimals <- unlist(lapply(decimals, "[", 2))
  decimals[is.na(decimals)] <- "0"
  if (is.null(digits)) {
    digits <- max(nchar(decimals))
  }
  numbers <- as.numeric(numbers)
  numbers <- sprintf(paste("%.", digits, "f", sep = ""), numbers)
  numbers <- as.numeric(numbers)

  return(numbers)
}



#' Makes names lowercase and replaces all punctuation with underscore.
#'
#' @param names
#' A string or vector of strings.
#'
#' @return
#' The inputted string or vector of strings now lowercased and
#' with punctuation replaced with a single underscore.
#' @export
#'
#' @examples
fix_column_names <- function(.names) {
  .names <- tolower(.names)
  .names <- gsub(" |-|\\/|\\.", "_", .names)
  .names <- gsub("_+", "_", .names)
  .names <- gsub("_$", "", .names)
  return(.names)
}





#' Prepare .tex output from kableExtra for Overleaf
#'
#' When kableExtra makes .tex files it adds lines to create a full .tex
#  document (e.g. \start{document}, and adds packages). This removes all that
#  stuff to it can be added to Overleaf.
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
#' @param words
#' A string or vector of strings with words you want capitalized
#' @param lowercase_of
#' If TRUE (default), keeps the string " of " to be lowercased as is custom in
#' English writing (e.g. District of Columbia).
#'
#' @return
#' The original string with the first letter of each word capitalized
#' @export
#'
#' @examples
capitalize_words <- function(words, lowercase_of = TRUE) {
  words <- tolower(words)
  words <- strsplit(words, " ")
  words <- lapply(words, upper_first_letter)
  words <- unlist(lapply(words, paste, collapse = " "))
  if (lowercase_of) {
    words <- gsub(" Of ", " of ", words)
  }
  return(words)
}

upper_first_letter <- function(word) {
  first_letter <- toupper(substring(word, 1, 1))
  other_letters <- substring(word, first = 2)
  word <- paste0(first_letter, other_letters)
  return(word)
}
