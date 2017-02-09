
#' crime
#'
#' @param dataset_name
#' Enter the name of the dataset you want to download. Full list of
#' datasets are found by entering data(available_crime_data)
#'
#'
#' @return
#' Data frame with crime data
#' @export
#'
#' @examples
#' crime_data("bloomington_use_force")
#'
#'



crime_data <- function(dataset_name) {

  stopifnot(is.character(dataset_name))

  assault_on_police_options <- c("hartford_assault_on_police",
                                 "louisville_assault_on_police",
                                 "montgomery_assault_on_police",
                                 "tucson_assault_on_police")

  calls_for_service_options <- c("baltimore_service",
                                 "bloomington_service",
                                 "burlington_service")

  use_of_force_options <- c("baltimore_use_force",
                            "bloomington_use_force",
                            "dallas_use_force",
                            "fayetteville_use_force",
                            "indianapolis_use_force",
                            "orlando_use_force",
                            "rutland_use_force")

  officer_shootings_options <- c("austin_OIS_2000_2014",
                                 "austin_OIS_2006_2015",
                                 "dallas_OIS",
                                 "hampton_OIS",
                                 "hartford_OIS",
                                 "indianapolis_OIS",
                                 "LA_OIS_people",
                                 "LA_OIS_animal",
                                 "philadelphia_OIS",
                                 "tucson_OIS")

  all_options <- append(officer_shootings_options,
                       use_of_force_options)
  all_options <- append(all_options, calls_for_service_options)
  all_options <- append(all_options, assault_on_police_options)

  if (dataset_name %in% assault_on_police_options) {
    message("This may take some time. Please be patient.")
    assault_on_police_cleaner(dataset_name)
  }
  else {
  if (dataset_name %in% calls_for_service_options) {
    message("This may take some time. Please be patient.")
    calls_for_service_cleaner(dataset_name)
  }
    else {
  if (dataset_name %in% officer_shootings_options) {
    message("This may take some time. Please be patient.")
    OIS_scraper_cleaner(dataset_name)
  }
      else {
  if (dataset_name %in% use_of_force_options) {
    message("This may take some time. Please be patient.")
    use_of_force_cleaner(dataset_name)
  }
        else {
    message(paste(dataset_name, "is not a valid input. Please try",
                  "another dataset",
                  sep = " "))
        }
      }
    }
  }
}
