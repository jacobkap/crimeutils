#' Pad decimal places with trailing zeros.
#'
#' @param numbers
#' A number or vector of numbers.
#' @param digits
#' Number of decimal places to pad. If NULL (default), uses the maximum
#' number of decimal places in the numbers input. If digits is less than
#' the number of decimal places in the data, rounds the data to the decimal
#' place specified. If rounding at a 5, follows R's rules to round to the
#' nearest even number.
#'
#' @return
#' The original numbers but with trailing zeros added to the decimal places.
#' @export
#'
#' @examples
#' pad_decimals(c(2, 3.4, 8.808))
pad_decimals <- function(numbers, digits = NULL) {

  if (!is.numeric(numbers)) {
    stop("The numbers parameter must be numeric.")
  }

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
#' @param .names
#' A string or vector of strings.
#'
#' @return
#' The inputted string or vector of strings now lowercase and
#' with punctuation replaced with a single underscore.
#' @export
#'
#' @examples
#' fix_column_names("BAD.-columnNAME")
fix_column_names <- function(.names) {

  if (!is.character(.names)) {
    .names <- as.character(.names)
  }


  .names <- tolower(.names)
  .names <- gsub(" |-|\\/|\\.", "_", .names)
  .names <- gsub("_+", "_", .names)
  .names <- gsub("_$", "", .names)
  return(.names)
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
#' capitalize_words("district of columbia")
capitalize_words <- function(words, lowercase_of = TRUE) {

  if (!is.character(words)) {
    words <- as.character(words)
  }

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
