OIS_scraper_cleaner <- function(dataset_name){

###### Officer involved shootings ######

### Austin Texas officer involved shootings

if (dataset_name == "austin_OIS_2000_2014") {

austin_OIS_2000_2014 <- read.socrata(
                                  paste("https://data.austintexas.gov/dataset",
                                        "/Officer-Involved-Shooting-2000-2014",
                                        "/63p6-iegi", sep = "")
                                        )
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

return(austin_OIS_2000_2014)
}

### Austin Texas officer involved shooting incidents 2006-2015

if (dataset_name == "austin_OIS_2006_2015"){

austin_OIS_2006_2015 <- read.socrata(
                                      paste("https://data.austintexas.gov/",
                                            "Public-Safety/OIS-Dataset-2006",
                                            "-15-Incidents/pjaq-d4i3",
                                            sep = "")
                                            )
austin_ois_officers_2006_2015 <- read.socrata(
                                          paste("https://data.austintexas.gov/",
                                                "Public-Safety/OIS-Dataset-2006",
                                                "-15-Officers/vhr5-vvw2",
                                                sep = "")
                                               )
austin_ois_subjects_2006_2015 <- read.socrata(
                                            paste("https://data.austintexas.gov",
                                                  "/Public-Safety/OIS-Dataset",
                                                  "-2006-15-Subjects/e9x2-49sw",
                                                  sep = "")
                                                )
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

return(austin_OIS_2006_2015)
}


### Dallas Texas officer involved shooting

if (dataset_name == "dallas_OIS"){

dallas_OIS <- read.socrata(paste("https://www.dallasopendata.com/",
                                  "Police/Dallas-Police-Public-Data",
                                  "-Officer-Involved-Shootin/4gmt-jyx2",
                                  sep = "")
                                 )

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

return(dallas_OIS)
}

### Hampton Virginia officer involved shootings

if (dataset_name == "hampton_OIS"){

hampton_OIS <- read.socrata(paste("https://data.hampton.gov/Government",
                                  "/Officer-Involved-Shootings/nhh2-ywzf",
                                  sep = ""))
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

return(hampton_OIS)
}

### Hartford Connecticut officer involved shooting

if (dataset_name == "hartford_OIS"){

hartford_OIS <- read.socrata(paste("https://data.hartford.gov/Public-Safety/",
                                   "Officer-Involved-Shooting-2005-Current/",
                                   "dzbp-kiee",
                                   sep = ""))
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

return(hartford_OIS)
}

### Indianapolis Indiana officer involved shooting

if (dataset_name == 'indianapolis_OIS'){

indianapolis_OIS <- read.csv(url(paste("https://www.projectcomport.org",
                                       "/department/1/ois.csv",
                                       sep = "")))
indianapolis_OIS$occurredDate <- ymd_hms(indianapolis_OIS$occurredDate)
names(indianapolis_OIS)[2] <- "date"
names(indianapolis_OIS)[8] <- "subject_weapon"
names(indianapolis_OIS)[9] <- "officer_weapon"
names(indianapolis_OIS)[10] <- "weapon_type"
names(indianapolis_OIS)[11] <- "subject_condition"
names(indianapolis_OIS)[12] <- "officer_condition"
names(indianapolis_OIS)[13] <- "subject_race_ethnicity"
names(indianapolis_OIS)[14] <- "subject_sex"
names(indianapolis_OIS)[15] <- "subject_age"
names(indianapolis_OIS)[16] <- "officer_race_ethnicity"
names(indianapolis_OIS)[17] <- "officer_sex"
names(indianapolis_OIS)[18] <- "officer_age"
names(indianapolis_OIS)[19] <- "officer_experience_years"

return(indianapolis_OIS)
}

### Knoxville Tennessee officer involved shooting


### Los Angeles California officer involved shooting

if (dataset_name == "LA_OIS"){

# Suspect Details For Deputy Involved Shooting Incidents (2010-YTD)
LA_OIS_1 <- read.socrata(paste("https://data.lacounty.gov/Criminal/",
                             "Person-Details-Hit-Shooting-Incidents",
                             "-and-Non-Hit-/t8vk-5p9p",
                             sep = ""))
# Deputy Details For Suspect Involved Shooting Incidents (2010-YTD)
LA_OIS_2 <- read.socrata(paste("https://data.lacounty.gov/Criminal/",
                               "Deputy-Details-Hit-Shooting-Incidents",
                               "-and-Non-Hit-/7jc3-gsk7",
                               sep = ""))
# Deputy Involved Shootings (2010-YTD)
LA_OIS_5 <- read.socrata(paste("https://data.lacounty.gov/Criminal/All-",
                               "Shooting-Incidents-for-Deputy-Involved-",
                               "Shootin/xutq-azb6",
                               sep = ""))
# Suspect Involved Shooting Incidents (2010-YTD)
LA_OIS_6 <- read.socrata(paste("https://data.lacounty.gov/Criminal/Hit-",
                               "Shooting-Incidents-and-Non-Hit-Shooting",
                               "-Incide/d6xt-ws3m",
                               sep = ""))


LA_OIS_1 <- LA_OIS_1[order(LA_OIS_1$INCIDENT.NUMBER),]
LA_OIS_2 <- LA_OIS_2[order(LA_OIS_2$INCIDENT.NUMBER),]
LA_OIS_3 <- LA_OIS_3[order(LA_OIS_3$INCIDENT.NUMBER),]
LA_OIS_4 <- LA_OIS_4[order(LA_OIS_4$INCIDENT.NUMBER),]
LA_OIS_5 <- LA_OIS_5[order(LA_OIS_5$INCIDENT.NUMBER),]
LA_OIS_6 <- LA_OIS_6[order(LA_OIS_6$INCIDENT.NUMBER),]

LA_OIS <- merge(LA_OIS_1, LA_OIS_2, by = c("INCIDENT.NUMBER"), suffixes = "zz")
LA_OIS <- merge(LA_OIS, LA_OIS_5, by = c("INCIDENT.NUMBER"), suffixes = "zz")
LA_OIS <- merge(LA_OIS, LA_OIS_6, by = c("INCIDENT.NUMBER"), suffixes = "zz")
names(LA_OIS) <- gsub("zz$|NA$", "", names(LA_OIS), ignore.case = TRUE)
uniquecol <- unique(names(LA_OIS))
LA_OIS_people <- LA_OIS[uniquecol]

names(LA_OIS_people) <- gsub("\\.", "_", names(LA_OIS_people))
names(LA_OIS_people) <- tolower(names(LA_OIS_people))
LA_OIS_people$address <- paste(LA_OIS_people$incident_location,
                               LA_OIS_people$city,
                               LA_OIS_people$state,
                               LA_OIS_people$zip,
                               sep = ", ")
LA_OIS_people$incident_location <- NULL
LA_OIS_people$state <- NULL
LA_OIS_people$zip <- NULL
LA_OIS_people$geo_location <- NULL

names(LA_OIS_people)[1] <- "case_number"
names(LA_OIS_people)[2] <- "call_type"
names(LA_OIS_people)[3] <- "date"
LA_OIS_people$date <- ymd_hms(LA_OIS_people$date)
LA_OIS_people$call_type <- as.character(LA_OIS_people$call_type)
LA_OIS_people$city <- as.character(LA_OIS_people$city)
names(LA_OIS_people)[6] <- "subject_age"
LA_OIS_people$person_race <- as.character(LA_OIS_people$person_race)
names(LA_OIS_people)[7] <- "subject_race"
names(LA_OIS_people)[16] <- "number_officers_present_during_shooting"
LA_OIS_people$mental_health_concerns <- gsub("N", 0,
                                    LA_OIS_people$mental_health_concerns,
                                    ignore.case = TRUE)
LA_OIS_people$mental_health_concerns <- gsub("Y", 1,
                                             LA_OIS_people$mental_health_concerns,
                                             ignore.case = TRUE)
LA_OIS_people$mental_health_concerns <-
                        as.numeric(LA_OIS_people$mental_health_concerns)

LA_OIS_people$criminal_history <- gsub("N", 0,
                                             LA_OIS_people$criminal_history,
                                             ignore.case = TRUE)
LA_OIS_people$criminal_history <- gsub("Y", 1,
                                             LA_OIS_people$criminal_history,
                                             ignore.case = TRUE)
LA_OIS_people$criminal_history <-
  as.numeric(LA_OIS_people$criminal_history)

LA_OIS_people$wounded <- gsub("N", 0,
                                             LA_OIS_people$wounded,
                                             ignore.case = TRUE)
LA_OIS_people$wounded <- gsub("Y", 1,
                                             LA_OIS_people$wounded,
                                             ignore.case = TRUE)
LA_OIS_people$wounded <-
  as.numeric(LA_OIS_people$wounded)

LA_OIS_people$deceased <- gsub("N", 0,
                                             LA_OIS_people$deceased,
                                             ignore.case = TRUE)
LA_OIS_people$deceased <- gsub("Y", 1,
                                             LA_OIS_people$deceased,
                                             ignore.case = TRUE)
LA_OIS_people$deceased <-
  as.numeric(LA_OIS_people$deceased)

LA_OIS_people$on_probation <- gsub("N", 0,
                                             LA_OIS_people$on_probation,
                                             ignore.case = TRUE)
LA_OIS_people$on_probation <- gsub("Y", 1,
                                             LA_OIS_people$on_probation,
                                             ignore.case = TRUE)
LA_OIS_people$on_probation <-   as.numeric(LA_OIS_people$on_probation)

LA_OIS_people$on_parole <- gsub("N", 0,
                                             LA_OIS_people$on_parole,
                                             ignore.case = TRUE)
LA_OIS_people$on_parole <- gsub("Y", 1,
                                             LA_OIS_people$on_parole,
                                             ignore.case = TRUE)
LA_OIS_people$on_parole <- as.numeric(LA_OIS_people$on_parole)

LA_OIS_people$under_the_influence <- gsub("N", 0,
                                             LA_OIS_people$under_the_influence,
                                             ignore.case = TRUE)
LA_OIS_people$under_the_influence <- gsub("Y", 1,
                                             LA_OIS_people$under_the_influence,
                                             ignore.case = TRUE)
LA_OIS_people$under_the_influence <-
  as.numeric(LA_OIS_people$under_the_influence)

LA_OIS_people$substance_name <- as.character(LA_OIS_people$substance_name)
LA_OIS_people$weapon_involved_category <-
        as.character(LA_OIS_people$weapon_involved_category)
LA_OIS_people$weapon_involved_category_desc <- as.character(
                         LA_OIS_people$weapon_involved_category_desc)

names(LA_OIS_people)[18] <- "shooting_description"
names(LA_OIS_people)[19] <- "longitude"
names(LA_OIS_people)[20] <- "latitude"
names(LA_OIS_people)[21] <- "number_of_subjects"
names(LA_OIS_people)[22] <- "officer_gender"
names(LA_OIS_people)[23] <- "officer_race"
names(LA_OIS_people)[24] <- "officer_age"
names(LA_OIS_people)[25] <- "officer_experience_years"
names(LA_OIS_people)[29] <- "number_of_previous_shootings"
names(LA_OIS_people)[34] <- "number_of_subjects_wounded"
names(LA_OIS_people)[35] <- "number_of_subjects_killed"

LA_OIS_people$shooting_description <- as.character(
                   LA_OIS_people$shooting_description)
LA_OIS_people$officer_gender <- gsub("F", "female",
                                     LA_OIS_people$officer_gender,
                                     ignore.case = TRUE)
LA_OIS_people$officer_gender <- gsub("m", "male",
                                     LA_OIS_people$officer_gender,
                                     ignore.case = TRUE)
LA_OIS_people$officer_gender <- as.character(LA_OIS_people$officer_gender)
LA_OIS_people$officer_race <- as.character(LA_OIS_people$officer_race)
LA_OIS_people$deputy_assigned_unit <-
               as.character(LA_OIS_people$deputy_assigned_unit)
LA_OIS_people$deputy_assigned_unit <-
               as.character(LA_OIS_people$deputy_assigned_unit_name)

LA_OIS_people$involved_in_previous_shootings <- gsub("y", 1,
                             LA_OIS_people$involved_in_previous_shootings,
                             ignore.case = TRUE)
LA_OIS_people$involved_in_previous_shootings <- gsub("n", 0,
                             LA_OIS_people$involved_in_previous_shootings,
                             ignore.case = TRUE)
LA_OIS_people$number_of_previous_shootings <- as.numeric(
              LA_OIS_people$number_of_previous_shootings)

LA_OIS_people$district_attorney_action <- as.character(
              LA_OIS_people$district_attorney_action)
LA_OIS_people$training <- gsub("N", 0, LA_OIS_people$training,
                               ignore.case = TRUE)
LA_OIS_people$training <- gsub("y", 1, LA_OIS_people$training,
                               ignore.case = TRUE)
LA_OIS_people$training <- as.numeric(LA_OIS_people$training)
LA_OIS_people$handling_unit_id <- as.character(LA_OIS_people$handling_unit_id)
LA_OIS_people$handling_unit_name <- as.character(
             LA_OIS_people$handling_unit_name)

return(LA_OIS_people)
}


 # LA shooting animal data

if (dataset_name == "LA_OIS_animal"){

  # Deputy Details For Non- Suspect Shooting Incidents (2010-YTD)
  LA_OIS_3 <- read.socrata(paste("https://data.lacounty.gov/Criminal/",
                                 "Deputy-Details-For-Non-Suspect-Shooting",
                                 "-Incidents-/9kqg-nmvi",
                                 sep = ""))
  # Non-Suspect Involved Shooting Incidents (2010-YTD)
  LA_OIS_4 <- read.socrata(paste("https://data.lacounty.gov/Criminal/Non-",
                                 "Person-Involved-Shooting-Incidents-2010-",
                                 "to-Pre/3q3t-7t2n",
                                 sep = ""))


LA_OIS_temp <- merge(LA_OIS_3, LA_OIS_4, by = "INCIDENT.NUMBER",
                     suffixes = "zz")
names(LA_OIS_temp) <- gsub("zz$|NA$", "", names(LA_OIS_temp),
                           ignore.case = TRUE)
uniquecol2 <- unique(names(LA_OIS_temp))
LA_OIS_animal <- LA_OIS_temp[uniquecol2]

LA_OIS_animal$INCIDENT.DATE <- ymd_hms(LA_OIS_animal$INCIDENT.DATE)
LA_OIS_animal$location <- paste(LA_OIS_animal$INCIDENT.LOCATION,
                                LA_OIS_animal$CITY,
                                LA_OIS_animal$STATE,
                                LA_OIS_animal$ZIP,
                                sep = ", ")
temp_city <- LA_OIS_animal$CITY
LA_OIS_animal$INCIDENT.LOCATION <- NULL
LA_OIS_animal$CITY <- NULL
LA_OIS_animal$STATE <- NULL
LA_OIS_animal$GEO_LOCATION <- NULL
LA_OIS_animal$ZIP <- NULL
names(LA_OIS_animal) <- gsub("\\.", "_", names(LA_OIS_animal))
names(LA_OIS_animal) <- tolower(names(LA_OIS_animal))
LA_OIS_animal$deputy_gender <- gsub("F", "female", LA_OIS_animal$deputy_gender,
                                    ignore.case = TRUE)
LA_OIS_animal$deputy_gender <- gsub("M", "male", LA_OIS_animal$deputy_gender,
                                    ignore.case = TRUE)
LA_OIS_animal$training <- gsub("N", "0", LA_OIS_animal$training,
                                    ignore.case = TRUE)
LA_OIS_animal$training <- gsub("1", "Y", LA_OIS_animal$training,
                                    ignore.case = TRUE)
# remove factor type
LA_OIS_animal$incident_type <- as.character(LA_OIS_animal$incident_type)
LA_OIS_animal$deputy_race <- as.character(LA_OIS_animal$deputy_race)
LA_OIS_animal$weapon_brand <- as.character(LA_OIS_animal$weapon_brand)
LA_OIS_animal$weapon_model <- as.character(LA_OIS_animal$weapon_model)
LA_OIS_animal$weapon_caliber <- as.character(LA_OIS_animal$weapon_caliber)
LA_OIS_animal$animal_type <- as.character(LA_OIS_animal$animal_type)
LA_OIS_animal$animal_breed <- as.character(LA_OIS_animal$animal_breed)
LA_OIS_animal$handling_unit_id <- as.character(LA_OIS_animal$handling_unit_id)
LA_OIS_animal$handling_unit_name <- as.character(
                        LA_OIS_animal$handling_unit_name)

names(LA_OIS_animal)[1] <- "case_number"
names(LA_OIS_animal)[2] <- "call_type"
names(LA_OIS_animal)[3] <- "date"
names(LA_OIS_animal)[5] <- "officer_gender"
names(LA_OIS_animal)[6] <- "officer_race"
names(LA_OIS_animal)[7] <- "officer_age"
names(LA_OIS_animal)[8] <- "officer_experience_years"
names(LA_OIS_animal)[16] <- "latitude"
names(LA_OIS_animal)[16] <- "longitude"
names(LA_OIS_animal)[19] <- "number_officers_present_during_shooting"
LA_OIS_animal$city <- temp_city

return(LA_OIS_animal)
}


### Louisville Kentucky officer involved shooting

### Philadelphia officer involved shooting

if (dataset_name == "philadelphia_OIS"){

philadelphia_OIS <- read.csv(url(paste("https://data.phila.gov/api/views/",
                              "jr6a-ctmq/rows.csv?accessType=DOWNLOAD",
                              sep = "")))

philadelphia_OIS$longitude <- gsub(".*(\\-.*) .*", "\\1", philadelphia_OIS$SHAPE)
philadelphia_OIS$latitude <- gsub(".* (.*)", "\\1", philadelphia_OIS$SHAPE)
philadelphia_OIS$latitude <- gsub(".$", "", philadelphia_OIS$latitude)
philadelphia_OIS$SHAPE <- NULL
philadelphia_OIS$DATE_ <- mdy_hms(philadelphia_OIS$DATE_)
philadelphia_OIS$date <- philadelphia_OIS$DATE_
philadelphia_OIS$DATE_ <- paste(philadelphia_OIS$DATE_,
                                philadelphia_OIS$TIME,
                                sep = " ")
philadelphia_OIS$date_time <- ymd_hm(philadelphia_OIS$DATE_)
philadelphia_OIS$DATE_ <- NULL
philadelphia_OIS$time <- NULL
names(philadelphia_OIS) <- tolower(names(philadelphia_OIS))
names(philadelphia_OIS) <- gsub("\\.", "_", names(philadelphia_OIS))
philadelphia_OIS$dist <- as.numeric(philadelphia_OIS$dist)
philadelphia_OIS$dc_key <- as.numeric(philadelphia_OIS$dc_key)
philadelphia_OIS$code <- as.numeric(philadelphia_OIS$code)
names(philadelphia_OIS)[5] <- "subject_race"
names(philadelphia_OIS)[6] <- "subject_sex"
names(philadelphia_OIS)[7] <- "subject_latino"
names(philadelphia_OIS)[8] <- "subject_age"
names(philadelphia_OIS)[9] <- "subject_wound"
names(philadelphia_OIS)[10] <- "shooting_inside"
names(philadelphia_OIS)[11] <- "shooting_outside"
names(philadelphia_OIS)[12] <- "shooting_fatal"

philadelphia_OIS$subject_race <- as.character(philadelphia_OIS$subject_race)
philadelphia_OIS$subject_race <- gsub("A", "asian",
                                      philadelphia_OIS$subject_race,
                                      ignore.case = TRUE)
philadelphia_OIS$subject_race <- gsub("B", "Black",
                                      philadelphia_OIS$subject_race,
                                      ignore.case = TRUE)
philadelphia_OIS$subject_race <- gsub("W", "White",
                                      philadelphia_OIS$subject_race,
                                      ignore.case = TRUE)
philadelphia_OIS$subject_race <- gsub("M", "unknown",
                                      philadelphia_OIS$subject_race,
                                      ignore.case = TRUE)

philadelphia_OIS$subject_sex <- as.character(philadelphia_OIS$subject_sex)
philadelphia_OIS$subject_sex <- gsub("F", "female",
                                      philadelphia_OIS$subject_sex,
                                      ignore.case = TRUE)
philadelphia_OIS$subject_sex <- gsub("m", "male",
                                      philadelphia_OIS$subject_sex,
                                      ignore.case = TRUE)
philadelphia_OIS$subject_sex <- gsub("B", "unknown",
                                      philadelphia_OIS$subject_sex,
                                      ignore.case = TRUE)

philadelphia_OIS$subject_latino <- gsub(-1, 1, philadelphia_OIS$subject_latino)
philadelphia_OIS$subject_wound <- as.character(philadelphia_OIS$subject_wound)
philadelphia_OIS$shooting_inside <- gsub(-1, 1,
                                         philadelphia_OIS$shooting_inside)
philadelphia_OIS$shooting_outside <- gsub(-1, 1,
                                          philadelphia_OIS$shooting_outside)
philadelphia_OIS$shooting_fatal <- gsub(-1, 1,
                                        philadelphia_OIS$shooting_fatal)

philadelphia_OIS$officer_involved <- gsub("N", 0, ignore.case = TRUE,
                                        philadelphia_OIS$officer_involved)
philadelphia_OIS$officer_involved <- gsub("Y", 1, ignore.case = TRUE,
                                        philadelphia_OIS$officer_involved)
philadelphia_OIS$officer_involved <- as.numeric(
                                  philadelphia_OIS$officer_involved)
philadelphia_OIS$offender_injured <- gsub("N", 0, ignore.case = TRUE,
                                          philadelphia_OIS$offender_injured)
philadelphia_OIS$offender_injured <- gsub("Y", 1, ignore.case = TRUE,
                                          philadelphia_OIS$offender_injured)
philadelphia_OIS$offender_injured <- as.numeric(
                                         philadelphia_OIS$offender_injured)
philadelphia_OIS$offender_deceased <- gsub("N", 0, ignore.case = TRUE,
                                          philadelphia_OIS$offender_deceased)
philadelphia_OIS$offender_deceased <- gsub("Y", 1, ignore.case = TRUE,
                                          philadelphia_OIS$offender_deceased)
philadelphia_OIS$offender_deceased <- as.numeric(
  philadelphia_OIS$offender_deceased)


philadelphia_OIS$location <- as.character(philadelphia_OIS$location)
philadelphia_OIS$police_districts <- as.numeric(
                              philadelphia_OIS$police_districts)

return(philadelphia_OIS)
}

###  San Francisco officer involved shooting

### Tucson Arizona officer involved shooting

if (dataset_name == "tucson_OIS"){

tucson_OIS <- read.csv(url(paste("http://gisdata.tucsonaz.gov/datasets/",
                           "37659dfb8b8346a4b3f579ad263abe5b_1.csv",
                           sep = "")))
tucson_OIS <- tucson_OIS[-c(1:13)]
tucson_OIS$DATASOURCE <- NULL
names(tucson_OIS)[1] <- "tucson_board_of_inquiry_number"
tucson_OIS$address <- paste(tucson_OIS$ADDRESS,
                            tucson_OIS$CITY,
                            tucson_OIS$STATE,
                            tucson_OIS$ZIP,
                            sep = ", ")
tucson_OIS$ADDRESS <- NULL
tucson_OIS$CITY <- NULL
tucson_OIS$STATE <- NULL
tucson_OIS$ZIP <- NULL
tucson_OIS$address <- gsub("^ ,.*", "", tucson_OIS$address)

 temp_coords <- geocode(tucson_OIS$address, source = "dsk")
 tucson_OIS$longitude <- temp_coords$lon
 tucson_OIS$latitude <- temp_coords$lat
 tucson_OIS$X_COORD <- NULL
 tucson_OIS$Y_COORD <- NULL

names(tucson_OIS) <- tolower(names(tucson_OIS))
names(tucson_OIS) <- gsub("\\.", "_", names(tucson_OIS))

for (i in 1:ncol(tucson_OIS)){
  tucson_OIS[,i] <- gsub("-----", NA, tucson_OIS[,i])
  }

tucson_OIS$inci_date <- ymd_hms(tucson_OIS$inci_date)

tucson_OIS$suspect_injured <- NA
tucson_OIS$suspect_injured[tucson_OIS$sus_injdec == "Deceased"] <- 1
tucson_OIS$suspect_injured[tucson_OIS$sus_injdec == "Injured"] <- 1
tucson_OIS$suspect_injured[tucson_OIS$sus_injdec == "No"] <- 0
tucson_OIS$suspect_injured[tucson_OIS$sus_injdec == "None"] <- 0

tucson_OIS$suspect_killed <- NA
tucson_OIS$suspect_killed[tucson_OIS$sus_injdec == "Deceased"] <- 1
tucson_OIS$suspect_killed[tucson_OIS$sus_injdec == "Injured"] <- 0
tucson_OIS$suspect_killed[tucson_OIS$sus_injdec == "No"] <- 0
tucson_OIS$suspect_killed[tucson_OIS$sus_injdec == "None"] <- 0
tucson_OIS$sus_injdec <- NULL

tucson_OIS$officer_injured <- NA
tucson_OIS$officer_injured[tucson_OIS$ofc_injdec == "Deceased"] <- 1
tucson_OIS$officer_injured[tucson_OIS$ofc_injdec == "Injured"] <- 1
tucson_OIS$officer_injured[tucson_OIS$ofc_injdec == "No"] <- 0
tucson_OIS$officer_injured[tucson_OIS$ofc_injdec == "None"] <- 0

tucson_OIS$officer_killed <- NA
tucson_OIS$officer_killed[tucson_OIS$ofc_injdec == "Deceased"] <- 1
tucson_OIS$officer_killed[tucson_OIS$ofc_injdec == "Injured"] <- 0
tucson_OIS$officer_killed[tucson_OIS$ofc_injdec == "No"] <- 0
tucson_OIS$officer_killed[tucson_OIS$ofc_injdec == "None"] <- 0
tucson_OIS$ofc_injdec <- NULL

names(tucson_OIS)[2] <- "date"
tucson_OIS$inci_num <- as.numeric(tucson_OIS$inci_num)
names(tucson_OIS)[3] <- "incident_number"

names(tucson_OIS)[4] <- "subject_weapon"
names(tucson_OIS)[5] <- "subject_age"
names(tucson_OIS)[6] <- "subject_race"
tucson_OIS$subject_race <- tolower(tucson_OIS$subject_race)
names(tucson_OIS)[7] <- "subject_gendr"
names(tucson_OIS)[8] <- "officer_age"
tucson_OIS$officer_age <- as.numeric(tucson_OIS$officer_age)
names(tucson_OIS)[9] <- "officer_race"
names(tucson_OIS)[10] <- "officer_gender"
names(tucson_OIS)[11] <- "officer_experience_years"
names(tucson_OIS)[12] <- "independent_police_auditor_outcome"
names(tucson_OIS)[13] <- "legal_outcome"
tucson_OIS$longitude <- as.numeric(tucson_OIS$longitude)
tucson_OIS$latitude <- as.numeric(tucson_OIS$latitude)

return(tucson_OIS)
}
}
