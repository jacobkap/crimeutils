###### Officer involved shootings ######

### Austin Texas officer involved shootings
austin_OIS_2000_2014_url <- paste("https://data.austintexas.gov/dataset",
                                  "/Officer-Involved-Shooting-2000-2014",
                                  "/63p6-iegi", sep = "")
austin_OIS_2000_2014 <- read.socrata(austin_OIS_2000_2014_url)
austin_OIS_2000_2014$Location_1 <- gsub(".*(\\(.*)", "\\1",
                                        austin_OIS_2000_2014$Location_1)
austin_OIS_2000_2014$Location_1 <- gsub("^.|.$|,", "",
                                        austin_OIS_2000_2014$Location_1)
austin_OIS_2000_2014$latitude <- gsub("(.*) .*", "\\1",
                                      austin_OIS_2000_2014$Location_1)
austin_OIS_2000_2014$latitude <- as.numeric(austin_OIS_2000_2014$latitude)
austin_OIS_2000_2014$longitude <- gsub(".* (.*)", "\\1",
                                       austin_OIS_2000_2014$Location_1)
austin_OIS_2000_2014$longitude <- as.numeric(austin_OIS_2000_2014$longitude)
austin_OIS_2000_2014$Location_1 <- NULL
names(austin_OIS_2000_2014)[1] <- "case_number"
names(austin_OIS_2000_2014)[5] <- "part_of_day"
names(austin_OIS_2000_2014)[6] <- "address"
austin_OIS_2000_2014$address <- street_cleaner(austin_OIS_2000_2014$address)
names(austin_OIS_2000_2014)[7] <- "location_type"
names(austin_OIS_2000_2014)[8] <- "inside_or_outside"
names(austin_OIS_2000_2014)[9] <- "call_type"
names(austin_OIS_2000_2014)[10] <- "subject_race_ethnicity"
names(austin_OIS_2000_2014)[11] <- "subject_gender"
names(austin_OIS_2000_2014)[12] <- "subject_age"
names(austin_OIS_2000_2014)[13] <- "subject_injuries"
names(austin_OIS_2000_2014)[14] <- "subject_weapon"
names(austin_OIS_2000_2014)[15] <- "number_officers_present_during_shooting"
names(austin_OIS_2000_2014)[16] <- "number_officers_shooting"

austin_OIS_2000_2014$Rank <- gsub("ofc", "officer", austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$Rank <- gsub("det", "detective", austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$Rank <- gsub("other - agt", "agent",
                                  austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$Rank <- gsub("other - dep", "sheriff_deputy",
                                  austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$Rank <- gsub("other agency - ofc",
                                  "offer_from_different_department",
                                  austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$Rank <- gsub("sgt", "sergeant", austin_OIS_2000_2014$Rank)
austin_OIS_2000_2014$officer_name <- gsub("^.* (.* .*)$", "\\1",
                                          austin_OIS_2000_2014$officer_name)


names(austin_OIS_2000_2014)[19] <- "officer_race_ethnicity"
names(austin_OIS_2000_2014)[20] <- "officer_gender"
names(austin_OIS_2000_2014)[21] <- "officer_age"
names(austin_OIS_2000_2014)[22] <- "department_jurisdiction"
names(austin_OIS_2000_2014)[23] <- "officer_experience_years"
names(austin_OIS_2000_2014)[24] <- "officer_weapon_caliber"
names(austin_OIS_2000_2014)[25] <- "officer_weapon_type"
names(austin_OIS_2000_2014)[26] <- "officer_shots_fired"
names(austin_OIS_2000_2014)[27] <- "officer_shots_hit"
names(austin_OIS_2000_2014)[28] <- "outcome"
names(austin_OIS_2000_2014) <- gsub("\\.", "_", names(austin_OIS_2000_2014))
names(austin_OIS_2000_2014) <- tolower(names(austin_OIS_2000_2014))


### Austin Texas officer involved shooting incidents 2006-2015
austin_OIS_2006_2015_url <- paste("https://data.austintexas.gov/",
                                  "Public-Safety/OIS-Dataset-2006",
                                  "-15-Incidents/pjaq-d4i3",
                                  sep = "")
austin_OIS_2006_2015_officers_url <- paste("https://data.austintexas.gov/",
                                           "Public-Safety/OIS-Dataset-2006",
                                           "-15-Officers/vhr5-vvw2",
                                           sep = "")
austin_OIS_2006_2015_subjects_url <- paste("https://data.austintexas.gov",
                                           "/Public-Safety/OIS-Dataset",
                                           "-2006-15-Subjects/e9x2-49sw",
                                           sep = "")
austin_OIS_2006_2015 <- read.socrata(austin_OIS_2006_2015_url)
austin_ois_officers_2006_2015 <- read.socrata(austin_OIS_2006_2015_officers_url)
austin_ois_subjects_2006_2015 <- read.socrata(austin_OIS_2006_2015_subjects_url)
austin_OIS_2006_2015 <- merge(austin_OIS_2006_2015,
                              austin_ois_officers_2006_2015, by = c("Case..",
                              "Date", "Location.1"))
austin_OIS_2006_2015 <- merge(austin_OIS_2006_2015, austin_ois_subjects_2006_2015,
                              by = c("Case..",
                                     "Date", "Location.1"))

austin_OIS_2006_2015$Location.1 <- gsub(".*\\(", "",
                                        austin_OIS_2006_2015$Location.1)
austin_OIS_2006_2015$Location.1 <- gsub(".$|,", "",
                                        austin_OIS_2006_2015$Location.1)
austin_OIS_2006_2015$latitude <- gsub("(.*) .*", "\\1",
                                      austin_OIS_2006_2015$Location.1)
austin_OIS_2006_2015$longitude <- gsub(".* (.*)", "\\1",
                                       austin_OIS_2006_2015$Location.1)
austin_OIS_2006_2015$Location.1 <- NULL
austin_OIS_2006_2015$latitude <- as.numeric(austin_OIS_2006_2015$latitude)
austin_OIS_2006_2015$longitude <- as.numeric(austin_OIS_2006_2015$longitude)

names(austin_OIS_2006_2015)[1] <- "case_number"
names(austin_OIS_2006_2015)[5] <- "part_of_day"
names(austin_OIS_2006_2015)[6] <- "location_type"
names(austin_OIS_2006_2015)[7] <- "inside_or_outside"
names(austin_OIS_2006_2015)[8] <- "subject_weapon"
names(austin_OIS_2006_2015)[9] <- "number_of_officers_shooting"
names(austin_OIS_2006_2015)[10] <- "call_type"
names(austin_OIS_2006_2015)[11] <- "number_officers_present_during_shooting"
names(austin_OIS_2006_2015)[12] <- "officer_shots_hit"
names(austin_OIS_2006_2015)[18] <- "department_jurisdiction"
names(austin_OIS_2006_2015)[19] <- "officer_experience_years"
names(austin_OIS_2006_2015)[20] <- "officer_involved_in_previous_shooting"
names(austin_OIS_2006_2015)[18] <- "officer_shots_fired"
names(austin_OIS_2006_2015)[23] <- "department_jurisdiction"
names(austin_OIS_2006_2015)[24] <- "outcome"


names(austin_OIS_2006_2015) <- tolower(names(austin_OIS_2006_2015))
names(austin_OIS_2006_2015) <- gsub(",|\\.", "_", names(austin_OIS_2006_2015))


austin_OIS_2006_2015$Rank <- gsub("ofc", "officer", austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$Rank <- gsub("det", "detective", austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$Rank <- gsub("other - agt", "agent",
                                  austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$Rank <- gsub("other - dep", "sheriff_deputy",
                                  austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$Rank <- gsub("other agency - ofc",
                                  "offer_from_different_department",
                                  austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$Rank <- gsub("sgt", "sergeant", austin_OIS_2006_2015$Rank)
austin_OIS_2006_2015$officer_name <- gsub("^.* (.* .*)$", "\\1",
                                          austin_OIS_2006_2015$officer_name)



### Dallas Texas officer involved shooting
dallas_OIS_url <- paste("https://www.dallasopendata.com/",
                        "Police/Dallas-Police-Public-Data",
                        "-Officer-Involved-Shootin/4gmt-jyx2",
                        sep = "")
dallas_OIS <- read.socrata(dallas_OIS_url)

dallas_OIS$GeoLocation <- gsub(".*\\(", "",
                              dallas_OIS$GeoLocation)
dallas_OIS$GeoLocation <- gsub(".$|,", "",
                              dallas_OIS$GeoLocation)
dallas_OIS$latitude <- gsub("(.*) .*", "\\1",
                            dallas_OIS$GeoLocation)
dallas_OIS$longitude <- gsub(".* (.*)", "\\1",
                             dallas_OIS$GeoLocation)
dallas_OIS$GeoLocation <- NULL
dallas_OIS$latitude <- as.numeric(dallas_OIS$latitude)
dallas_OIS$longitude <- as.numeric(dallas_OIS$longitude)
names(dallas_OIS)[1] <- "case_number"
names(dallas_OIS)[3] <- "address"
names(dallas_OIS)[4] <- "subject_injuries"

dallas_OIS$Subject.s. <- as.character(dallas_OIS$Subject.s.)
dallas_OIS$subject_1 <- NA
dallas_OIS$subject_2 <- NA
dallas_OIS$subject_3 <- NA

temp_split <- strsplit(dallas_OIS$Subject.s., split = ";")
for (i in 1:length(temp_split)){
  dallas_OIS$subject_1[i] <- temp_split[[i]][[1]]
  if (length(temp_split[[i]]) == 2) {
    dallas_OIS$subject_2[i] <- temp_split[[i]][[2]]
  }
  if (length(temp_split[[i]]) == 3) {
    dallas_OIS$subject_3[i] <- temp_split[[i]][[3]]
  }
}
dallas_OIS$Subject.s. <- NULL

dallas_OIS$subject_1_race <- NA
dallas_OIS$subject_1_sex <- NA
dallas_OIS$subject_2_race <- NA
dallas_OIS$subject_2_sex <- NA
dallas_OIS$subject_3_race <- NA
dallas_OIS$subject_3_sex <- NA

# race
dallas_OIS$subject_1_race <- gsub(".*(.)..$", "\\1", dallas_OIS$subject_1)
dallas_OIS$subject_2_race <- gsub(".*(.)..$", "\\1", dallas_OIS$subject_2)
dallas_OIS$subject_3_race <- gsub(".*(.)..$", "\\1", dallas_OIS$subject_3)
dallas_OIS$subject_1_race <- gsub("a", "asian", dallas_OIS$subject_1_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_1_race <- gsub("\\/", "unknown", dallas_OIS$subject_1_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_1_race <- gsub("b", "black", dallas_OIS$subject_1_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_1_race <- gsub("l", "hispanic", dallas_OIS$subject_1_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_1_race <- gsub("w", "white", dallas_OIS$subject_1_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_2_race <- gsub("a", "asian", dallas_OIS$subject_2_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_2_race <- gsub("\\/", "unknown", dallas_OIS$subject_2_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_2_race <- gsub("b", "black", dallas_OIS$subject_2_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_2_race <- gsub("l", "hispanic", dallas_OIS$subject_2_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_2_race <- gsub("w", "white", dallas_OIS$subject_2_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_3_race <- gsub("a", "asian", dallas_OIS$subject_3_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_3_race <- gsub("\\/", "unknown", dallas_OIS$subject_3_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_3_race <- gsub("b", "black", dallas_OIS$subject_3_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_3_race <- gsub("l", "hispanic", dallas_OIS$subject_3_race,
                                  ignore.case = TRUE)
dallas_OIS$subject_3_race <- gsub("w", "white", dallas_OIS$subject_3_race,
                                  ignore.case = TRUE)
# sex
dallas_OIS$subject_1_sex <- gsub(".*..(.)$", "\\1", dallas_OIS$subject_1)
dallas_OIS$subject_2_sex <- gsub(".*..(.)$", "\\1", dallas_OIS$subject_2)
dallas_OIS$subject_3_sex <- gsub(".*..(.)$", "\\1", dallas_OIS$subject_3)
dallas_OIS$subject_1_sex <- gsub("F", "female", dallas_OIS$subject_1_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_1_sex <- gsub("M", "male", dallas_OIS$subject_1_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_1_sex <- gsub("y", "unknown", dallas_OIS$subject_1_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_2_sex <- gsub("F", "female", dallas_OIS$subject_2_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_2_sex <- gsub("M", "male", dallas_OIS$subject_2_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_2_sex <- gsub("y", "unknown", dallas_OIS$subject_2_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_3_sex <- gsub("F", "female", dallas_OIS$subject_3_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_3_sex <- gsub("M", "male", dallas_OIS$subject_3_sex,
                                 ignore.case = TRUE)
dallas_OIS$subject_3_sex <- gsub("y", "unknown", dallas_OIS$subject_3_sex,
                                 ignore.case = TRUE)

names(dallas_OIS)[6] <- "officer_name"
names(dallas_OIS) <- tolower(names(dallas_OIS))
names(dallas_OIS) <- gsub(",|\\.", "_", names(dallas_OIS))



### Hampton Virginia officer involved shootings
hampton_OIS_url <- paste("https://data.hampton.gov/Government",
                         "/Officer-Involved-Shootings/nhh2-ywzf",
                         sep = "")
hampton_OIS <- read.socrata(hampton_OIS_url)
names(hampton_OIS)[1] <- "case_number"
names(hampton_OIS)[2] <- "date"
hampton_OIS$date <- mdy_hms(hampton_OIS$Date.and.Time)
names(hampton_OIS) <- tolower(names(hampton_OIS))
names(hampton_OIS) <- gsub(",|\\.", "_", names(hampton_OIS))
hampton_OIS$address <- paste(hampton_OIS$location, "hampton, virginia",
                             sep = " ")
hampton_OIS$address <- gsub("blk", "block", hampton_OIS$address,
                            ignore.case = TRUE)
hampton_OIS$address <- gsub("W\\.", "west", hampton_OIS$address,
                            ignore.case = TRUE)
hampton_OIS$address <- gsub("s\\.", "south", hampton_OIS$address,
                            ignore.case = TRUE)
hampton_OIS$address <- gsub("n\\.", "north", hampton_OIS$address,
                            ignore.case = TRUE)
hampton_OIS$address <- gsub("e\\.", "east", hampton_OIS$address,
                            ignore.case = TRUE)
hampton_OIS$location <- NULL


### Hartford Connecticut officer involved shooting
hartford_OIS_url <- paste("https://data.hartford.gov/Public-Safety/",
                          "Officer-Involved-Shooting-2005-Current/",
                          "dzbp-kiee",
                          sep = "")
hartford_OIS <- read.socrata(hartford_OIS_url)
hartford_OIS$Time_24HR <- hour_cleaner(hartford_OIS$Time_24HR)
hartford_OIS$Date_time <- paste(hartford_OIS$Date,
                           hartford_OIS$Time_24HR)
hartford_OIS$Date_time <- mdy_hm(hartford_OIS$Date_time)
hartford_OIS$Date <- mdy(hartford_OIS$Date)
hartford_OIS$Address <- paste(hartford_OIS$Address,
                              "hartford, connecticut",
                              sep = " ")
hartford_OIS$geom <- gsub("^.|,|.$", "", hartford_OIS$geom)
hartford_OIS$latitude <- gsub("(.*) .*", "\\1",
                              hartford_OIS$geom)
hartford_OIS$longitude <- gsub(".* (.*)", "\\1",
                               hartford_OIS$geom)
hartford_OIS$geom <- NULL
hartford_OIS$latitude <- as.numeric(hartford_OIS$latitude)
hartford_OIS$longitude <- as.numeric(hartford_OIS$longitude)
names(hartford_OIS) <- tolower(names(hartford_OIS))
names(hartford_OIS) <- gsub(",|\\.", "_", names(hartford_OIS))


### Indianapolis Indiana officer involved shooting
