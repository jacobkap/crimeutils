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

#' Returns abbreviations of state name input.
#'
#' @param state
#' A vector of strings with the names of US states.
#'
#' @return
#' A vector of strings with the abbreviations of the inputted state names.
#' @export
#'
#' @examples
#' make_state_abb("california")
make_state_abb <- function(state) {
  state_abb <- datasets::state.abb[match(tolower(state),
                                         tolower(datasets::state.name))]
  state_abb[tolower(state) == "canal zone"]           <- "CZ"
  state_abb[tolower(state) == "district of columbia"] <- "DC"
  state_abb[tolower(state) == "guam"]                 <- "GU"
  state_abb[tolower(state) == "puerto rico"]          <- "PR"
  return(state_abb)
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


  words <- strsplit(words, "-")
  words <- lapply(words, upper_first_letter)
  words <- unlist(lapply(words, paste, collapse = "-"))


  words <- strsplit(words, "'")
  words <- lapply(words, upper_first_letter)
  words <- unlist(lapply(words, paste, collapse = "'"))

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


# For some reason a bunch of the datasets end certain columns with '_x" so this
# just removes it to be cleaner (personal choice, not really important)
drop_underscore_x_from_name <- function(x) {
  x <- gsub("_x$", "", x)
  return(x)
}
