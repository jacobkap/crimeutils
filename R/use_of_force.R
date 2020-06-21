# # https://www.policedatainitiative.org/datasets/use-of-force/
#
# get_use_of_force_baltimore <- function() {
#   data <- data.table::fread("https://data.baltimorecity.gov/api/views/3w4d-kckv/rows.csv?accessType=DOWNLOAD")
#   data <-
#     data %>%
#     dplyr::rename_all(fix_column_names) %>% # Function from this package to clean column names
#   dplyr::mutate_if(is.character, tolower) %>% # Make all character types consistent - lower case
#     dplyr::rename(longitude = x_long,
#                   latitude  = y_lat) %>% # Make lat and long column names consistent in all data
#     dplyr::mutate(date = lubridate::mdy_hms(date)) %>%
#     dplyr::select(-coordinates,
#                   date,
#                   tidyr::everything()) %>% # Since there's already lat and lon columns, don't need coordinates column. Make dates the first columns
#     dplyr::arrange(desc(date)) # Sort by most recent date on top
#
#   return(data)
# }
#
#
# get_use_of_force_cincinnati <- function() {
#   data <- data.table::fread("https://data.cincinnati-oh.gov/api/views/8us8-wi2w/rows.csv?accessType=DOWNLOAD")
#   data <-
#     data %>%
#     dplyr::rename_all(fix_column_names) %>% # Function from this package to clean column names
#     dplyr::mutate_if(is.character, tolower) %>% # Make all character types consistent - lower case
#     dplyr::rename(longitude = longitude_x,
#                   latitude  = latitude_x,
#                   date_time = incident_date) %>% # Make lat and long column names consistent in all data
#     dplyr::mutate(date_time = lubridate::mdy_hms(date_time),
#                   date      = lubridate::floor_date(date_time, unit = "day")) %>%
#     dplyr::select(date,
#                   date_time,
#                   tidyselect::everything()) %>% # Since there's already lat and lon columns, don't need coordinates column. Make dates the first columns
#     dplyr::arrange(desc(date_time)) # Sort by most recent date on top
#
#   return(data)
# }
#
#
# get_use_of_force_austin <- function() {
#   data <- data.table::fread("https://data.austintexas.gov/api/views/iydp-s2cf/rows.csv?accessType=DOWNLOAD")
#   data <-
#     data %>%
#     dplyr::rename_all(fix_column_names) %>% # Function from this package to clean column names
#     dplyr::mutate_if(is.character, tolower) %>% # Make all character types consistent - lower case
#     dplyr::rename(longitude = x_coordinate,
#                   latitude  = y_coordinate,
#                   date = date_occurred) %>% # Make lat and long column names consistent in all data
#     dplyr::mutate(date = lubridate::mdy_hms(date),
#                   officer_commission_date = lubridate::mdy_hms(officer_commission_date)) %>%
#     dplyr::select(date,
#                   tidyselect::everything()) %>% # Since there's already lat and lon columns, don't need coordinates column. Make dates the first columns
#     dplyr::arrange(desc(date)) # Sort by most recent date on top
#
#   return(data)
# }
#
