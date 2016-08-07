library(RSocrata)
library(jsonlite)
library(lubridate)
library(stringr)

######## Assault on police ########

### Hartford Connecticut assault on police
hartford_assault_on_polic_url <- paste("https://data.hartford.gov/Public-Safety/",
                                       "Assault-on-a-Police-Officer-2005",
                                       "-Current/p3tu-ygwc", sep = "")
hartford_assault_on_police <- read.socrata(hartford_assault_on_polic_url)
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



### Louisville Kentucky assault on police
louisville_assault_on_police_url <- paste("http://api.louisvilleky.gov/api/File/",
                                          "DownloadFile?fileName=Assaulted",
                                          "OfficerData.csv",
                                          sep = "")
louisville_assault_on_police <- read.csv(url(louisville_assault_on_police_url))
louisville_assault_on_police$DATE_REPORTED <-
                         ymd_hms(louisville_assault_on_police$DATE_REPORTED)
louisville_assault_on_police$DATE_OCCURED <-
  ymd_hms(louisville_assault_on_police$DATE_OCCURED)
names(louisville_assault_on_police) <- tolower(names(louisville_assault_on_police))
names(louisville_assault_on_police)[12] <- "address"


### Montgomery Maryland assault on police
montgomery_assault_on_police_url <- paste("https://data.montgomerycountymd.gov",
                                          "/Public-Safety/Assaults-on-Officers/",
                                          "dhdu-k59t", sep = "")
montgomery_assault_on_police <- read.socrata(montgomery_assault_on_police_url)
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


#### Tuscon Ariona assault on police
tuscon_assault_on_police_url <- paste("http://gisdata.tucsonaz.gov/datasets/",
                                      "16aa6ea45f4e40a5a629ee6da98618fd_0.csv",
                                      sep = "")
tuscon_assault_on_police <- read.csv(url(tuscon_assault_on_police_url))
names(tuscon_assault_on_police)[1] <- "longitude"
names(tuscon_assault_on_police)[2] <- "latitude"
tuscon_assault_on_police$OBJECTID <- NULL
tuscon_assault_on_police$xcoord <- NULL
tuscon_assault_on_police$ycoord <- NULL
tuscon_assault_on_police$offender_r <- as.character(
                 tuscon_assault_on_police$offender_r)
tuscon_assault_on_police$offender_s <- as.character(
  tuscon_assault_on_police$offender_s)
tuscon_assault_on_police$offender_w <- as.character(
  tuscon_assault_on_police$offender_w)
tuscon_assault_on_police$street <- street_cleaner(tuscon_assault_on_police$street)
tuscon_assault_on_police$address <- paste(tuscon_assault_on_police$streetnbr,
                                          tuscon_assault_on_police$street,
                                          tuscon_assault_on_police$city,
                                          tuscon_assault_on_police$state,
                                          tuscon_assault_on_police$zip,
                                          sep = " ")
tuscon_assault_on_police$streetnbr <- NULL
tuscon_assault_on_police$street <- NULL
tuscon_assault_on_police$address <- address_cleaner(
                     tuscon_assault_on_police$address)
names(tuscon_assault_on_police)[8] <- "reported_as"
names(tuscon_assault_on_police)[9] <- "charged_as"
names(tuscon_assault_on_police)[10] <- "arrest_charge"
tuscon_assault_on_police$hour_occu <- hour_cleaner(
               tuscon_assault_on_police$hour_occu)
tuscon_assault_on_police$date_occu <- ymd_hms(tuscon_assault_on_police$date_occu)
tuscon_assault_on_police$date_time <- paste(tuscon_assault_on_police$date_occu,
                                            tuscon_assault_on_police$hour_occu)
tuscon_assault_on_police$date_time <- ymd_hm(tuscon_assault_on_police$date_time)
names(tuscon_assault_on_police)[3] <- "incident_id"
names(tuscon_assault_on_police)[11] <- "charge_description"
names(tuscon_assault_on_police)[12] <- "date_occurred"
names(tuscon_assault_on_police)[13] <- "hour_occurred"
names(tuscon_assault_on_police)[14] <- "division_sector"
names(tuscon_assault_on_police)[15] <- "offender_age"
names(tuscon_assault_on_police)[16] <- "offender_race"
names(tuscon_assault_on_police)[17] <- "offender_sex"
names(tuscon_assault_on_police)[18] <- "offender_weapon"
