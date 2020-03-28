# https://www.policedatainitiative.org/datasets/use-of-force/

get_use_of_force_baltimore <- function() {
  data <- data.table::fread("https://data.baltimorecity.gov/api/views/3w4d-kckv/rows.csv?accessType=DOWNLOAD")
  data <-
    data %>%
    dplyr::rename_all(fix_column_names) %>% # Function from this package to clean column names
  dplyr::mutate_if(is.character, tolower) %>% # Make all character types consistent - lower case
    dplyr::rename(longitude = x_long,
                  latitude  = y_lat) %>% # Make lat and long column names consistent in all data
    dplyr::mutate(date = lubridate::mdy_hms(date)) %>%
    dplyr::select(-coordinates,
                  date,
                  tidyr::everything())# Since there's already lat and lon columns, don't need coordinates column. Make dates the first columns

  return(data)
}

