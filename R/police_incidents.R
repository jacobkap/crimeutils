# Police Incident Data

# Baltimore Maryland incident data
baltimore_incidents <- function() {

  baltimore_incident <- data.table::fread(paste0("https://data.",
             "baltimorecity.gov/api/views/wsfq-mvij/rows.csv?accessType",
             "=DOWNLOAD"), strip.white = TRUE, showProgress = FALSE,
                                          check.names = TRUE)
  baltimore_incident <- data.frame(baltimore_incident)



  # Make longitude and latitude columns
  baltimore_incident <- coordinate_splitter(baltimore_incident,
                                            "Location.1")

  # Fix time
  baltimore_incident$CrimeTime  <- stringr::str_replace(
    baltimore_incident$CrimeTime,
    ":", "")
  baltimore_incident$CrimeTime  <- stringr::str_replace(
    baltimore_incident$CrimeTime,
    ":.*", "")

  # Make crime data time
  baltimore_incident$CrimeTime <- paste0(baltimore_incident$CrimeDate, " ",
                                         baltimore_incident$CrimeTime)
  baltimore_incident$CrimeTime <- lubridate::mdy_hm(baltimore_incident$CrimeTime)

  # Turn date into Date format
  baltimore_incident$CrimeDate <- mdy(baltimore_incident$CrimeDate)
  baltimore_incident <- date_column_maker(baltimore_incident, "CrimeTime")

  return(baltimore_incident)
}


philadelphia_incidents <- function() {
  philadelphia_incident <- data.table::fread(paste0("https://data.phila.gov/api/views",
                                                    "/sspu-uyfa/rows.csv?accessType=DOWNLOAD"),
                                             strip.white = TRUE, showProgress = FALSE,
                                             check.names = TRUE)


  philadelphia_incident <- coordinate_splitter(philadelphia_incident, "Shape")
  return(philadelphia_incidents)
}


coordinate_splitter <- function(dataset, column_name) {
  if (!is.character(column_name)) {
    stop(paste0("coordinate_column_name is the name of your",
                " column with the coordinate data.",
         " It must be a character!"))
  }

  names(dataset) <- gsub(column_name, "coordinates_column",
                                       names(dataset))

  system.time(dataset[, coordinates_column :=
            gsub("([:digits:-])|[[:alpha:]]|[[:punct:]]",
            "\\1", coordinates_column, perl = TRUE)])


  dataset[, coordinates_column := stringi::stri_trim(coordinates_column)]


  dataset$longitude <- stringr::str_split_fixed(
    dataset[, column_name],
    " ", 2)[,1]
  dataset$latitude <- stringr::str_split_fixed(
    dataset[, column_name],
    " ", 2)[,2]

  dataset[, column_name] <- NULL

  return(dataset)
}

# Makes year, month, day, weekday, and hour columns
date_column_maker <- function(dataset, date_column_name, time = TRUE) {
  dataset$year <- lubridate::year(dataset[, date_column_name])
  dataset$month <- lubridate::month(dataset[, date_column_name],
                                    label = TRUE, abbr = FALSE)
  dataset$day <- lubridate::day(dataset[, date_column_name])
  dataset$weekday <- lubridate::wday(dataset[, date_column_name],
                                     label = TRUE, abbr = FALSE)

  if (time) {
    dataset$hour <- lubridate::hour(dataset[, date_column_name])
  }

  return(dataset)
}
