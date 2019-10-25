#' Get ORIs that consistently report their data every year.
#'
#'
#' @param data
#' A data.frame with Uniform Crime Report (UCR) data. Requires at least
#' the ORI, year, and number_of_months_reported columns.
#' @param minimum_months_reported
#' Integer indicating the minimum number of months requesting to keep in data.
#'
#' @return
#' A vector with the ORIs that report the minimum number of months for every
#' year in the data.
#' @export
#'
#' @examples
ucr_constant_reporter_oris <- function(data,
                                       minimum_months_reported) {

  names(data) <- tolower(names(data))
  if (!minimum_months_reported %in% 0:12) {
    stop("minimum_months_reported must be between 0 and 12.")
  }
  constant_oris <- c()
  data <-
    data %>%
    dplyr::filter(number_of_months_reported >= minimum_months_reported) %>%
    dplyr::select(ori,
                  year,
                  number_of_months_reported)


  for (i in unique(data$year)) {
    temp <-
      data %>%
      dplyr::filter(year %in% i)
    if (length(constant_oris) == 0) {
      constant_oris <- c(constant_oris, unique(temp$ori))
    } else {
      constant_oris <- constant_oris[constant_oris %in% unique(temp$ori)]
    }
  }

  return(constant_oris)
}
