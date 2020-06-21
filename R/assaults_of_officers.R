# # https://www.policedatainitiative.org/datasets/assaults-on-officers/
#
# get_assault_on_officers_bloomington <- function() {
#
#
#   return(data)
# }
#
#
#
# get_assault_on_officers_cincinnati <- function() {
# # https://data.cincinnati-oh.gov/Safety/PDI-Police-Data-Initiative-Assaults-on-Officers/bmmy-avxm/data
#   data <- data.table::fread("https://data.cincinnati-oh.gov/api/views/bmmy-avxm/rows.csv?accessType=DOWNLOAD")
#   data <-
#     data %>%
#     dplyr::rename_all(fix_column_names) %>%
#     dplyr::rename_all(drop_underscore_x_from_name) %>%
#     dplyr::mutate_if(is.character, tolower) %>%
#     dplyr::rename(date_time_reported = date_reported) %>%
#     dplyr::mutate(date_time_reported = lubridate::mdy_hms(date_time_reported),
#                   date_reported = lubridate::floor_date(date_time_reported,
#                                                         unit = "day")) %>%
#     dplyr::select(date_time_reported,
#                   date_reported,
#                   tidyr::everything()) %>%
#     dplyr::arrange(desc(date_time_reported))
#
#   return(data)
# }
#
#
# get_assault_on_officers_hartford <- function() {
#
#
#   return(data)
# }
#
#
# get_assault_on_officers_lincoln <- function() {
#
#
#   return(data)
# }
#
#
# get_assault_on_officers_louisville <- function() {
#
#
#   return(data)
# }
#
#
# get_assault_on_officers_montgomery_county <- function() {
#
#
#   return(data)
# }
#
#
# get_assault_on_officers_salt_lake_city <- function() {
#
#
#   return(data)
# }
#
#
# get_assault_on_officers_tuscon <- function() {
#
#
#   return(data)
# }
