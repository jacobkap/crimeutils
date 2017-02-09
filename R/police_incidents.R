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
  baltimore_incident <- date_column_maker(baltimore_incident)

  return(baltimore_incident)
}


#' Philadelphia Incidents
#'
#' @return
#' data.frame with all available Philly crime data
#' @export
#'
#' @examples
philadelphia_incidents <- function() {
  philadelphia_incident <- data.table::fread(paste0(
    "https://data.phila.gov/api/views",
    "/sspu-uyfa/rows.csv?accessType=DOWNLOAD"),
     strip.white = TRUE, showProgress = FALSE,
     check.names = TRUE)

  philadelphia_incident <- coordinate_splitter(philadelphia_incident, "Shape")
  philadelphia_incident <- smart_date(philadelphia_incident, "Dispatch.Date")
  philadelphia_incident <- date_column_maker(philadelphia_incident)
  philadelphia_incident <- philadelphia_incident[order(date_column, decreasing = TRUE)]

  philadelphia_incident <- data.frame(philadelphia_incident)
  return(philadelphia_incident)
}


coordinate_splitter <- function(dataset, column_name) {
  if (!is.character(column_name)) {
    stop(paste0("coordinate_column_name is the name of your",
                " column with the coordinate data.",
         " It must be a character!"))
  }

  names(dataset) <- gsub(column_name, "coordinates_column",
                                       names(dataset))

  dataset[, coordinates_column :=
            gsub("([:digits:-])|[[:alpha:]]|[[:punct:]]",
            "\\1", coordinates_column, perl = TRUE)]
  dataset[, coordinates_column := stringi::stri_trim(coordinates_column)]


  dataset[, c("longitude", "latitude") :=
            list(stringr::str_split_fixed(coordinates_column,
                                          "\\s+", 2)[,1],
                stringr::str_split_fixed(coordinates_column,
                                           "\\s+", 2)[,2])]

  return(dataset)
}

# Makes year, month, day, weekday, and hour columns
date_column_maker <- function(dataset) {

  dataset[, c("year", "month", "day", "weekday") :=
            list(year(date_column),
                 month(date_column, label = TRUE, abbr = FALSE),
                 day(date_column),
                 wday(date_column, label = TRUE, abbr = FALSE))]


  return(dataset)
}

smart_date <- function(dataset, column_name) {
  if (!is.character(column_name)) {
    stop("column name must be the name of the date column - as a string")
  }

  if (!column_name %in% names(dataset)) {
    stop("column name must be the name of the date column - spelled exactly")
  }

  names(dataset) <- gsub(column_name, "date_column",
                         names(dataset))

  if (nrow(dataset) > 2000) {
    sample_rows <- sample(1:nrow(dataset), 100, replace = FALSE)
  } else (sample_rows <- nrow(dataset))

  library(lubridate)

  # Auto selects correct lubridate order for dates (no times)
  if (!testit::has_warning(mdy(dataset[sample_rows, date_column]))) {

    return(dataset[, date_column := mdy(date_column)]) # month day year
  } else if (!testit::has_warning(myd(dataset[sample_rows, date_column]))) {
    return(dataset[, date_column := myd(date_column)]) # month year day
  } else if (!testit::has_warning(ymd(dataset[sample_rows, date_column]))) {
    return(dataset[, date_column := ymd(date_column)]) # year month day
  } else if (!testit::has_warning(ydm(dataset[sample_rows, date_column]))) {
    return(dataset[, date_column := ydm(date_column)]) # year day month
  } else if (!testit::has_warning(dmy(dataset[sample_rows, date_column]))) {
    return(dataset[, date_column := dmy(date_column)]) # day month year
  } else if (!testit::has_warning(dym(dataset[sample_rows, date_column]))) {
    return(dataset[, date_column := dym(date_column)]) # day year month
  }

}

