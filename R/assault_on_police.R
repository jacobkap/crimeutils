assault_on_police_cleaner <- function(dataset_name){


######## Assault on police ########

### Hartford Connecticut assault on police

if (dataset_name == "hartford_assault_on_police"){

hartford_assault_on_police <- RSocrata::read.socrata(
                      paste("https://data.hartford.gov/Public-Safety/",
                            "Assault-on-a-Police-Officer-2005",
                            "-Current/p3tu-ygwc", sep = ""))
hartford_assault_on_police$UCR_1_Category <-
  gsub(".*- (.*)", "\\1", hartford_assault_on_police$UCR_1_Category)
hartford_assault_on_police$UCR_2_Category <-
  gsub(".*- (.*)", "\\1", hartford_assault_on_police$UCR_2_Category)
hartford_assault_on_police$geom <- gsub("^.|.$", "",
                                        hartford_assault_on_police$geom)
hartford_assault_on_police$longitude <- as.numeric(gsub(".* (.*)", "\\1",
                                       hartford_assault_on_police$geom))
hartford_assault_on_police$latitude <- as.numeric(gsub("(.*), .*", "\\1",
                                                  hartford_assault_on_police$geom))
hartford_assault_on_police$geom <- NULL
hartford_assault_on_police$Time_24HR <- gsub("(.*)(..$)", "\\1:\\2",
                                             hartford_assault_on_police$Time_24HR)
hartford_assault_on_police$date_time <- paste(hartford_assault_on_police$Date,
                                              hartford_assault_on_police$Time_24HR,
                                              sep = " ")
hartford_assault_on_police$date_time <- mdy_hm(hartford_assault_on_police$date_time)
hartford_assault_on_police$Date <- mdy(hartford_assault_on_police$Date)
names(hartford_assault_on_police) <- tolower(names(hartford_assault_on_police))

hartford_assault_on_police$department_name <- "hartford_police"

return(hartford_assault_on_police)
}


### Louisville Kentucky assault on police

if (dataset_name == "louisville_assault_on_police"){

louisville_assault_on_police <- readr::read_csv(url(
                                    paste("http://api.louisvilleky.gov/api/File/",
                                          "DownloadFile?fileName=Assaulted",
                                          "OfficerData.csv",
                                          sep = "")
                                ))
louisville_assault_on_police$DATE_REPORTED <-
                         ymd_hms(louisville_assault_on_police$DATE_REPORTED)
louisville_assault_on_police$DATE_OCCURED <-
  ymd_hms(louisville_assault_on_police$DATE_OCCURED)
names(louisville_assault_on_police) <- tolower(names(louisville_assault_on_police))
names(louisville_assault_on_police)[12] <- "address"

louisville_assault_on_police$department_name <- "louisville_metro_police"

return(louisville_assault_on_police)
}

### Montgomery Maryland assault on police

if (dataset_name == "montgomery_assault_on_police"){

montgomery_assault_on_police <- RSocrata::read.socrata(
                      paste("https://data.montgomerycountymd.gov",
                            "/Public-Safety/Assaults-on-Officers/",
                            "dhdu-k59t", sep = "")
                            )
montgomery_assault_on_police$Dispatch.Date...Time <-
      mdy_hms(montgomery_assault_on_police$Dispatch.Date...Time)
names(montgomery_assault_on_police)[2] <- "dispatch_date"
montgomery_assault_on_police$Start.Date...Time <-
  mdy_hms(montgomery_assault_on_police$Start.Date...Time)
names(montgomery_assault_on_police)[14] <- "start_date"
montgomery_assault_on_police$End.Date...Time <-
  mdy_hms(montgomery_assault_on_police$End.Date...Time)
names(montgomery_assault_on_police)[15] <- "end_date"
montgomery_assault_on_police$address <- paste(
                montgomery_assault_on_police$Block.Address,
                montgomery_assault_on_police$City,
                montgomery_assault_on_police$State,
                montgomery_assault_on_police$Zip.Code)

montgomery_assault_on_police$Block.Address <- NULL
montgomery_assault_on_police$State <- NULL
montgomery_assault_on_police$Location <- NULL
names(montgomery_assault_on_police) <- tolower(names(montgomery_assault_on_police))
names(montgomery_assault_on_police) <- gsub("\\.", "_",
                                            names(montgomery_assault_on_police))

montgomery_assault_on_police$department_name <- "montgomery_county_police"

return(montgomery_assault_on_police)
}

#### Tucson Ariona assault on police

if (dataset_name == "tucson_assault_on_police"){

tucson_assault_on_police <- readr::read_csv(url(
                        paste("http://gisdata.tucsonaz.gov/datasets/",
                              "16aa6ea45f4e40a5a629ee6da98618fd_0.csv",
                              sep = "")))
names(tucson_assault_on_police)[1] <- "longitude"
names(tucson_assault_on_police)[2] <- "latitude"
tucson_assault_on_police$OBJECTID <- NULL
tucson_assault_on_police$xcoord <- NULL
tucson_assault_on_police$ycoord <- NULL
tucson_assault_on_police$offender_r <- as.character(
                 tucson_assault_on_police$offender_r)
tucson_assault_on_police$offender_s <- as.character(
  tucson_assault_on_police$offender_s)
tucson_assault_on_police$offender_w <- as.character(
  tucson_assault_on_police$offender_w)
tucson_assault_on_police$street <- street_cleaner(tucson_assault_on_police$street)
tucson_assault_on_police$address <- paste(tucson_assault_on_police$streetnbr,
                                          tucson_assault_on_police$street,
                                          tucson_assault_on_police$city,
                                          tucson_assault_on_police$state,
                                          tucson_assault_on_police$zip,
                                          sep = " ")
tucson_assault_on_police$streetnbr <- NULL
tucson_assault_on_police$street <- NULL
tucson_assault_on_police$address <- address_cleaner(
                     tucson_assault_on_police$address)
names(tucson_assault_on_police)[8] <- "reported_as"
names(tucson_assault_on_police)[9] <- "charged_as"
names(tucson_assault_on_police)[10] <- "arrest_charge"
tucson_assault_on_police$hour_occu <- hour_cleaner(
               tucson_assault_on_police$hour_occu)
tucson_assault_on_police$date_occu <- ymd_hms(tucson_assault_on_police$date_occu)
tucson_assault_on_police$date_time <- paste(tucson_assault_on_police$date_occu,
                                            tucson_assault_on_police$hour_occu)
tucson_assault_on_police$date_time <- ymd_hm(tucson_assault_on_police$date_time)
names(tucson_assault_on_police)[3] <- "incident_id"
names(tucson_assault_on_police)[11] <- "charge_description"
names(tucson_assault_on_police)[12] <- "date_occurred"
names(tucson_assault_on_police)[13] <- "hour_occurred"
names(tucson_assault_on_police)[14] <- "division_sector"
names(tucson_assault_on_police)[15] <- "offender_age"
names(tucson_assault_on_police)[16] <- "offender_race"
names(tucson_assault_on_police)[17] <- "offender_sex"
names(tucson_assault_on_police)[18] <- "offender_weapon"

tucson_assault_on_police$department_name <- "tucson_police_department"
return(tucson_assault_on_police)
}
}
