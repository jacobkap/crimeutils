#' Title
#'
#' @param data
#' @param start_year
#' @param end_year
#' @param minimum_months_reported
#'
#' @return
#' @export
#'
#' @examples
ucr_constant_reporter_oris <- function(data,
                                       start_year,
                                       end_year,
                                       minimum_months_reported) {

  if (!minimum_months_reported %in% 0:12) {
    stop()
  }
  constant_oris <- c()
  data <-
    data %>%
    dplyr::filter(year %in% start_year:end_year,
                  number_of_months_reported >= minimum_months_reported) %>%
    dplyr::select(ori,
                  year,
                  number_of_months_reported)


  for (i in start_year:end_year) {
    temp <-
      data %>%
      dplyr::filter(year %in% i)
    if (i %in% start_year) {
      constant_oris <- c(constant_oris, unique(temp$ori))
    } else {
      constant_oris <- constant_oris[constant_oris %in% unique(temp$ori)]
    }
  }

  return(constant_oris)
}
