use_of_force_cleaner <- function(dataset_name){


##### Austin Texas use of force #####



##### Baltimore Maryland use of force ####
if (dataset_name == "baltimore_use_force") {

system.time(baltimore_use_force <- read.socrata("https://data.baltimorecity.gov/resource/4ih5-d5d5.json"))

names(baltimore_use_force) <- tolower(names(baltimore_use_force))
baltimore_use_force$date <- ymd(baltimore_use_force$date)
baltimore_use_force$coordinates <- NULL

names(baltimore_use_force)[2] <- "incident_number"
names(baltimore_use_force)[3] <- "police_district"
names(baltimore_use_force)[4] <- "address"
names(baltimore_use_force)[5] <- "force_type"
names(baltimore_use_force)[6] <- "longitude"
names(baltimore_use_force)[7] <- "latitude"

baltimore_use_force$force_type <- as.character(baltimore_use_force$force_type)
baltimore_use_force$incident_number <- as.numeric(
                           baltimore_use_force$incident_number)
baltimore_use_force$police_district <- as.numeric(
                           baltimore_use_force$police_district)
baltimore_use_force$address <- street_cleaner(baltimore_use_force$address)
baltimore_use_force$address <- paste(baltimore_use_force$address,
                                     "Baltimore",
                                     "MD",
                                     sep = ", ")

baltimore_use_force$department_name <- "baltimore_police"

return(baltimore_use_force)
}

#### Bloomington Indiana use of force ####

if (dataset_name == "bloomington_use_force"){


bloomington_use_force1 <- read_csv(url(paste("https://data.bloomington.",
                        "in.gov/dataset/93c0b51f-f6b6-475e-b2f1-852c4f5c",
                        "6eb7/resource/a3e4c4c3-dc1f-4246-bfe3-41baa85aa4",
                        "66/download/2016-first-quarter-use-of-force.csv",
                        sep = "")),
                        skip = 1)

bloomington_use_force2 <- read_csv(url(paste("https://data.bloomington.in.",
                         "gov/dataset/93c0b51f-f6b6-475e-b2f1-852c4f5c6eb7/",
                         "resource/6928d2c5-0469-417e-84c8-9292cac5511e/",
                         "download/2016-second-quarter-use-of-force.csv",
                                   sep = "")),
                                   skip = 1)


bloomington_use_force1$X <- NULL
bloomington_use_force1$X.1 <- NULL

bloomington_use_force1 <- bloomington_use_force1[
                    !bloomington_use_force1$Case.Number == "",]
bloomington_use_force2 <- bloomington_use_force2[
  !bloomington_use_force2$Case.Number == "",]

names(bloomington_use_force2)[29] <- names(bloomington_use_force1)[29]

bloomington_use_force <- rbind(bloomington_use_force1,
                               bloomington_use_force2)
rm(bloomington_use_force1)
rm(bloomington_use_force2)

bloomington_use_force$date <- mdy(bloomington_use_force$Date)
bloomington_use_force$date_time <- paste(bloomington_use_force$Date,
                                         bloomington_use_force$Time,
                                         sep = " ")
bloomington_use_force$date_time <- mdy_hm(bloomington_use_force$date_time)
bloomington_use_force$Date <- NULL
bloomington_use_force$Time <- NULL

bloomington_use_force$Physical.Contact <- binary_cleaner(
                     bloomington_use_force$Physical.Contact)
bloomington_use_force$Less.Lethal <- binary_cleaner(
  bloomington_use_force$Less.Lethal)
bloomington_use_force$Draw.Weapon <- binary_cleaner(
  bloomington_use_force$Draw.Weapon)
bloomington_use_force$Weapon.Fired <- binary_cleaner(
  bloomington_use_force$Weapon.Fired)
bloomington_use_force$Vehicle.Pursuit <- binary_cleaner(
  bloomington_use_force$Vehicle.Pursuit)
bloomington_use_force$Foot.Pursuit <- binary_cleaner(
  bloomington_use_force$Foot.Pursuit)
bloomington_use_force$Suspect.Armed <- binary_cleaner(
  bloomington_use_force$Suspect.Armed)
bloomington_use_force$Suspect.Injured <- binary_cleaner(
  bloomington_use_force$Suspect.Injured)
bloomington_use_force$Officer.Injured <- binary_cleaner(
  bloomington_use_force$Officer.Injured)
bloomington_use_force$Officer.Exposed.to.BBP <- binary_cleaner(
  bloomington_use_force$Officer.Exposed.to.BBP)

bloomington_use_force$Case.Number <- as.character(
                    bloomington_use_force$Case.Number)
bloomington_use_force$District <- as.character(
  bloomington_use_force$District)
bloomington_use_force$Nature <- as.character(
  bloomington_use_force$Nature)
bloomington_use_force$How.Received <- as.character(
  bloomington_use_force$How.Received)
bloomington_use_force$Suspect.Condition <- as.character(
  bloomington_use_force$Suspect.Condition)
bloomington_use_force$Suspect.Role <- as.character(
  bloomington_use_force$Suspect.Role)

bloomington_use_force$Caucasian..Non.Hispanic[is.na(
  bloomington_use_force$Caucasian..Non.Hispanic)] <- 0
bloomington_use_force$Caucasian..Hispanic[is.na(
                              bloomington_use_force$Caucasian..Hispanic)] <- 0
bloomington_use_force$African.American..Non.Hispanic[is.na(
  bloomington_use_force$African.American..Non.Hispanic)] <- 0
bloomington_use_force$African.American..Hispanic[is.na(
  bloomington_use_force$African.American..Hispanic)] <- 0
bloomington_use_force$Indian.Alaskan.Native..Non.Hispanic[is.na(
  bloomington_use_force$Indian.Alaskan.Native..Non.Hispanic)] <- 0
bloomington_use_force$Indian.Alaskan.Native..Hispanic[is.na(
  bloomington_use_force$Indian.Alaskan.Native..Hispanic)] <- 0
bloomington_use_force$Asian.Pacific.Island..Non.Hispanic[is.na(
  bloomington_use_force$Asian.Pacific.Island..Non.Hispanic)] <- 0
bloomington_use_force$X.Asian.Pacific.Island..Hispanic[is.na(
  bloomington_use_force$X.Asian.Pacific.Island..Hispanic)] <- 0

bloomington_use_force$subject_black <-
  (bloomington_use_force$African.American..Hispanic +
  bloomington_use_force$African.American..Non.Hispanic)
bloomington_use_force$subject_white <-
  (bloomington_use_force$Caucasian..Hispanic +
  bloomington_use_force$Caucasian..Non.Hispanic)
bloomington_use_force$subject_hispanic <-
  (bloomington_use_force$African.American..Hispanic +
  bloomington_use_force$Caucasian..Hispanic)
bloomington_use_force$subject_asian <-
  (bloomington_use_force$Indian.Alaskan.Native..Hispanic +
  bloomington_use_force$Indian.Alaskan.Native..Non.Hispanic +
  bloomington_use_force$Asian.Pacific.Island..Non.Hispanic +
  bloomington_use_force$X.Asian.Pacific.Island..Hispanic)

bloomington_use_force$African.American..Hispanic <- NULL
bloomington_use_force$African.American..Non.Hispanic <- NULL
bloomington_use_force$Caucasian..Non.Hispanic <- NULL
bloomington_use_force$Caucasian..Hispanic <- NULL
bloomington_use_force$Indian.Alaskan.Native..Hispanic <- NULL
bloomington_use_force$Indian.Alaskan.Native..Non.Hispanic <- NULL
bloomington_use_force$Asian.Pacific.Island..Non.Hispanic <- NULL
bloomington_use_force$X.Asian.Pacific.Island..Hispanic <- NULL

bloomington_use_force$Suspect.Gender.Female[is.na(
  bloomington_use_force$Suspect.Gender.Female)] <- 0
bloomington_use_force$Suspect.Gender.Male[is.na(
  bloomington_use_force$Suspect.Gender.Male)] <- 0

bloomington_use_force$Suspect.Age <- as.numeric(as.character(
  bloomington_use_force$Suspect.Age))

names(bloomington_use_force) <- gsub("\\.", "_", names(bloomington_use_force))
names(bloomington_use_force) <- tolower(names(bloomington_use_force))

names(bloomington_use_force)[3] <- "call_type"
names(bloomington_use_force)[4] <- "call_origin"
names(bloomington_use_force)[7] <- "officer_drew_weapon"
names(bloomington_use_force)[8] <- "officer_weapon_fired"
names(bloomington_use_force)[11] <- "subject_condition"
names(bloomington_use_force)[12] <- "subject_armed"
names(bloomington_use_force)[13] <- "subject_injured"
names(bloomington_use_force)[14] <- "subject_role"
names(bloomington_use_force)[16] <- "officer_exposed_to_blood_borne_pathogen"
names(bloomington_use_force)[17] <- "suspect_female"
names(bloomington_use_force)[18] <- "suspect_male"

bloomington_use_force$department_name <- "bloomington_police"

return(bloomington_use_force)
}

#### Dallas Texas use of force ####

if (dataset_name == "dallas_use_force") {

dallas_use_force_2014 <- read.socrata(paste("https://www.dallasopendata.com/",
                        "dataset/Police-2014-Response-to-Resistance/xiv3-e8g7",
                        sep = ""))

dallas_use_force_2015 <- read.socrata(paste("https://www.dallasopendata.com/",
                        "dataset/Police-2015-Response-to-Resistance/594v-2cnd",
                        sep = ""))

dallas_use_force_2014$ARC_Street <- street_cleaner(
         dallas_use_force_2014$ARC_Street)
dallas_use_force_2014$address <- paste(dallas_use_force_2014$ARC_Street,
                                       "Dallas, Texas",
                                       sep = ", ")

x1 <- geocode(dallas_use_force_2014$address[1:2000], source = "dsk")
x2 <- geocode(dallas_use_force_2014$address[2001:3396], source = "dsk")
x3 <- rbind(x1, x2)
rm(x1)
rm(x2)
dallas_use_force_2014$longitude <- x3$lon
dallas_use_force_2014$latitude <- x3$lat
dallas_use_force_2014$OBJECTID <- NULL
dallas_use_force_2014$ARC_Street <- NULL
rm(x3)

dallas_use_force_2015$ARC_Street <- street_cleaner(
  dallas_use_force_2015$ARC_Street)
dallas_use_force_2015$address <- paste(dallas_use_force_2015$ARC_Street,
                                       "Dallas, Texas",
                                       sep = ", ")

dallas_use_force_2015$Longitude <- as.numeric(gsub(".*\\(.*, (.*).", "\\1",
                                        dallas_use_force_2015$GeoLocation))
dallas_use_force_2015$Latitude <- as.numeric(gsub(".*\\((.*), .*", "\\1",
                                  dallas_use_force_2015$GeoLocation))
dallas_use_force_2015$GeoLocation <- NULL
dallas_use_force_2015$ID <- NULL
dallas_use_force_2015$ARC_Street <- NULL


dallas_use_force_2015$Latitude[is.na(dallas_use_force_2015$Latitude)] <-
     geocode(dallas_use_force_2015$address, source = "dsk")[2]

for(i in 1:nrow(dallas_use_force_2015)){
  if (is.na(dallas_use_force_2015$Latitude[i]) |
      is.na(dallas_use_force_2015$Longitude[i])){
    dallas_use_force_2015$Latitude[i] <- geocode(
                                         dallas_use_force_2015$address[i],
                                         source = "dsk")[2]
    dallas_use_force_2015$Longitude[i] <- geocode(
                                          dallas_use_force_2015$address[i],
                                          source = "dsk")[1]
    }
}

dallas_use_force_2015$longitude <- dallas_use_force_2015$Longitude
dallas_use_force_2015$Longitude <- NULL
dallas_use_force_2015$latitude <- dallas_use_force_2015$Latitude
dallas_use_force_2015$Latitude <- NULL

names(dallas_use_force_2015) <- names(dallas_use_force_2014)

dallas_use_force <- rbind(dallas_use_force_2014,
                          dallas_use_force_2015)
rm(dallas_use_force_2014)
rm(dallas_use_force_2015)
names(dallas_use_force) <- tolower(names(dallas_use_force))

dallas_use_force <- dallas_use_force[order(dallas_use_force$occurred_d),]
rownames(dallas_use_force) <- 1:nrow(dallas_use_force)

dallas_use_force$date <- ymd(dallas_use_force$occurred_d)
dallas_use_force$date_time <- paste(dallas_use_force$occurred_d,
                                    dallas_use_force$occurred_tm,
                                    sep = " ")
dallas_use_force$date_time <- ymd_hm(dallas_use_force$date_time)
dallas_use_force$occurred_d <- NULL
dallas_use_force$occurred_tm <- NULL
dallas_use_force$officer_experience_years <-   round(difftime(
                                               dallas_use_force$date,
                                               dallas_use_force$hire_dt,
                                               units = "days")
                                               /365, digits = 3)
dallas_use_force$hire_dt <- NULL
names(dallas_use_force)[1] <- "file_number"
names(dallas_use_force)[2] <- "officer_badge_number"
names(dallas_use_force)[3] <- "officer_sex"
names(dallas_use_force)[4] <- "officer_race"
names(dallas_use_force)[5] <- "officer_injuries"
names(dallas_use_force)[6] <- "officer_condition"
names(dallas_use_force)[7] <- "officer_to_hospital"
names(dallas_use_force)[8] <- "call_type"
names(dallas_use_force)[9] <- "use_of_force_number"
names(dallas_use_force)[10] <- "force_type"
names(dallas_use_force)[11] <- "reason_for_force"
dallas_use_force$cycles_num <- NULL
names(dallas_use_force)[12] <- "was_force_effective"
names(dallas_use_force)[13] <- "subject_number"
names(dallas_use_force)[14] <- "subject_race"
names(dallas_use_force)[15] <- "subject_sex"
names(dallas_use_force)[16] <- "subject_injuries"
names(dallas_use_force)[17] <- "subject_condition"
names(dallas_use_force)[18] <- "subject_arrest"
names(dallas_use_force)[19] <- "subject_intoxicated_mentally_ill"
names(dallas_use_force)[20] <- "subject_charges"
names(dallas_use_force)[25] <- "district_name"

dallas_use_force <- defactor(dallas_use_force)
dallas_use_force$officer_badge_number <- as.numeric(
                    dallas_use_force$officer_badge_number)
dallas_use_force$ra <- as.numeric(
  dallas_use_force$ra)
dallas_use_force$beat <- as.numeric(
  dallas_use_force$beat)
dallas_use_force$sector <- as.numeric(
  dallas_use_force$sector)
dallas_use_force$longitude <- as.numeric(
  dallas_use_force$longitude)
dallas_use_force$latitude <- as.numeric(
  dallas_use_force$latitude)
dallas_use_force$date <- as.Date(dallas_use_force$date)
dallas_use_force$date_time <- as.Date(dallas_use_force$date_time)
dallas_use_force <- dallas_use_force[!is.na(
                        dallas_use_force$officer_badge_number) &
                          !is.na(dallas_use_force$longitude),]

dallas_use_force$department_name <- "dallas_police"

return(dallas_use_force)
}

#### Fayetteville North Carolina use of force ####

if (dataset_name == "fayetteville_use_force") {

fayetteville_use_force <- read_csv(url(paste("http://data.fayettevillenc.gov/",
                          "datasets/c99349a76b924a80bb16f93585614c86_0.csv",
                          sep = "")))
fayetteville_use_force$date <- ymd_hms(fayetteville_use_force$Incident_Date)
fayetteville_use_force$Incident_Date <- NULL
fayetteville_use_force$longitude <- fayetteville_use_force[,1]
fayetteville_use_force$latitude <- fayetteville_use_force$Y
fayetteville_use_force[,1]
fayetteville_use_force$Y <- NULL
fayetteville_use_force$address <- paste(fayetteville_use_force$Location,
                                        fayetteville_use_force$City,
                                        fayetteville_use_force$State)
fayetteville_use_force$Location <- NULL
fayetteville_use_force$City <- NULL
fayetteville_use_force$State <- NULL
fayetteville_use_force$FID <- NULL

names(fayetteville_use_force) <- tolower(names(fayetteville_use_force))
fayetteville_use_force$subject_race <- gsub("^(.).*", "\\1",
                                      fayetteville_use_force$complaints_r_s)
fayetteville_use_force$subject_sex <- gsub(".*(.$)", "\\1",
                                            fayetteville_use_force$complaints_r_s)
fayetteville_use_force$complaints_r_s <- NULL
fayetteville_use_force$subject_sex <- gender_cleaner(
                                      fayetteville_use_force$subject_sex)
fayetteville_use_force$subject_race <- race_cleaner(
                                       fayetteville_use_force$subject_race)

# makes all the officer age columns
officer_age_split = strsplit(as.character(
                           fayetteville_use_force$officers_age), "/")

officer_age_split <- as.data.frame(do.call(rbind,lapply(
                     officer_age_split, `length<-`,
                     max(sapply(officer_age_split, length)))))

colnumber <- ncol(fayetteville_use_force)
age_names <- 1:ncol(officer_age_split)
  for(i in 1:ncol(officer_age_split)){
  fayetteville_use_force[[colnumber + i]] <- officer_age_split[[i]]
  age_names[i] <- paste("officer_",
                     i,
                     "_age",
                     sep = "")
}

names(fayetteville_use_force)[(ncol(fayetteville_use_force) -
                               (ncol(officer_age_split) -1)):
                              ncol(fayetteville_use_force)] <- age_names


# Makes all the officer race columns
officer_race_split = strsplit(as.character(
  fayetteville_use_force$officers_r_s), " +")

officer_race_split <- as.data.frame(do.call(rbind,lapply(
  officer_race_split, `length<-`,
  max(sapply(officer_race_split, length)))))

colnumber <- ncol(fayetteville_use_force)
race_names <- 1:ncol(officer_race_split)
for(i in 1:max(sapply(officer_age_split, length))){
  officer_race <- gsub("(.).*", "\\1", officer_race_split[[i]])
  officer_race <- race_cleaner(officer_race)
  fayetteville_use_force[[colnumber + i]] <- officer_race
  race_names[i] <- paste("officer_",
                     i,
                     "_race",
                     sep = "")
}

names(fayetteville_use_force)[(ncol(fayetteville_use_force)-
                                (ncol(officer_race_split) -1)):
                              ncol(fayetteville_use_force)] <- race_names

# makes all the officer sex columns
sex_names <- 1:ncol(officer_race_split)
for(i in 1:max(sapply(officer_age_split, length))){
  officer_sex <- gsub(".*(.)", "\\1", officer_race_split[[i]])
  officer_sex <- gender_cleaner(officer_sex)
  fayetteville_use_force[[colnumber + i]] <- officer_sex
  race_names[i] <- paste("officer_",
                         i,
                         "_race",
                         sep = "")
}

names(fayetteville_use_force)[(ncol(fayetteville_use_force)-
                                 (ncol(officer_race_split) -1)):
                                ncol(fayetteville_use_force)] <- sex_names

fayetteville_use_force$officers_age <- NULL
fayetteville_use_force$officers_r_s <- NULL
names(fayetteville_use_force)[1] <- "file_number"
names(fayetteville_use_force)[2] <- "subject_age"

fayetteville_use_force$department_name <- "fayetteville_police"

return(fayetteville_use_force)
}

#### Indianapolis Indiana use of force ####

if (dataset_name == "indianapolis_use_force"){

indianapolis_use_force <- read_csv(url(paste("https://www.projectcomport.org/",
                                       "department/1/uof.csv",
                                       sep = "")))

indianapolis_use_force <- defactor(indianapolis_use_force)
indianapolis_use_force$date <- ymd_hms(indianapolis_use_force$occurredDate)
indianapolis_use_force$occurredDate <- NULL
indianapolis_use_force$shift <- gsub("^SW|^SE|^NE|^ND|^ED|^WD|^SD|^ND|^NW",
                                     "", indianapolis_use_force$shift,
                                     ignore.case = TRUE)

indianapolis_use_force$officerInjured <- binary_cleaner(
                                         indianapolis_use_force$officerInjured)
indianapolis_use_force$officerHospitalized <- binary_cleaner(
                      indianapolis_use_force$officerHospitalized)
indianapolis_use_force$residentAge <- as.numeric(
                      indianapolis_use_force$residentAge)
indianapolis_use_force$officerAge <- as.numeric(
                      indianapolis_use_force$residentAge)
indianapolis_use_force$officerYearsOfService <- as.numeric(
                      indianapolis_use_force$officerYearsOfService)
indianapolis_use_force$arrestMade <- as.numeric(
                      indianapolis_use_force$arrestMade)

names(indianapolis_use_force)[1] <- "file_number"
names(indianapolis_use_force)[2] <- "use_force_reason"
names(indianapolis_use_force)[3] <- "force_type"
names(indianapolis_use_force)[9] <- "call_type"
names(indianapolis_use_force)[10] <- "arrest_made"
names(indianapolis_use_force)[11] <- "arrest_charges"
names(indianapolis_use_force)[12] <- "subject_injured"
names(indianapolis_use_force)[13] <- "subject_hospitalized"
names(indianapolis_use_force)[14] <- "subject_condition"
names(indianapolis_use_force)[15] <- "officer_injured"
names(indianapolis_use_force)[16] <- "officer_hospitalized"
names(indianapolis_use_force)[17] <- "officer_condition"
names(indianapolis_use_force)[18] <- "subject_race"
names(indianapolis_use_force)[19] <- "subject_gender"
names(indianapolis_use_force)[20] <- "subject_age"
names(indianapolis_use_force)[21] <- "officer_race"
names(indianapolis_use_force)[22] <- "officer_gender"
names(indianapolis_use_force)[23] <- "officer_age"
names(indianapolis_use_force)[24] <- "officer_experience_years"
names(indianapolis_use_force)[25] <- "officer_identifier"

indianapolis_use_force$department_name <- "indianapolis_police"

return(indianapolis_use_force)
}

#### Orlando Florida use of force ####

if (dataset_name == "orlando_use_force"){

orlando_use_force <- fromJSON(paste("https://data.cityoforlando.net/",
                                    "resource/vt8u-kse4.json",
                                    sep = ""))

orlando_use_force$date_time <- ymd_hms(orlando_use_force$incident_date_time)
orlando_use_force$incident_date_time <- NULL

orlando_use_force$chemical_agent_used <- binary_cleaner(
                      orlando_use_force$chemical_agent_used)
orlando_use_force$deflation_device_used <- binary_cleaner(
                      orlando_use_force$deflation_device_used)
orlando_use_force$electronic_device_used <- binary_cleaner(
                      orlando_use_force$electronic_device_used)
orlando_use_force$impact_weapons_used <- binary_cleaner(
                      orlando_use_force$impact_weapons_used)
orlando_use_force$k9_unit_involved <- binary_cleaner(
                      orlando_use_force$k9_unit_involved)
orlando_use_force$offender_injured <- binary_cleaner(
                      orlando_use_force$offender_injured)
orlando_use_force$offender_medical_treatment <- binary_cleaner(
                      orlando_use_force$offender_medical_treatment)
orlando_use_force$officer_battered <- binary_cleaner(
                      orlando_use_force$officer_battered)
orlando_use_force$officer_injured <- binary_cleaner(
                      orlando_use_force$offender_injured)
orlando_use_force$offender_medical_treatment <- binary_cleaner(
                      orlando_use_force$offender_medical_treatment)
orlando_use_force$physical_strikes_made <- binary_cleaner(
                      orlando_use_force$physical_strikes_made)
orlando_use_force$tackle_take_down <- binary_cleaner(
                      orlando_use_force$tackle_take_down)
orlando_use_force$`:@computed_region_bgqw_styj` <- NULL
orlando_use_force$`:@computed_region_gsfg_ku74` <- NULL
orlando_use_force$`:@computed_region_u8wz_9eai` <- NULL
orlando_use_force$location$type <- NULL

orlando_use_force$location$coordinates <- as.character(
                            orlando_use_force$location$coordinates)
orlando_use_force$address <- paste(street_cleaner(
                             orlando_use_force$incident_location),
                             "Orlando, FL",
                             sep = ", ")
orlando_use_force$incident_location <- NULL

orlando_use_force$longitude <- gsub(".*(-.*), .*", "\\1",
                                    orlando_use_force$location$coordinates)
orlando_use_force$longitude <- gsub("NULL", NA, orlando_use_force$longitude)
orlando_use_force$latitude <- gsub(".*, (.*).", "\\1",
                                    orlando_use_force$location$coordinates)
orlando_use_force$latitude <- gsub("NULL", NA, orlando_use_force$latitude)
orlando_use_force$location <- NULL

orlando_use_force$witnesses_involved <- as.numeric(
                             orlando_use_force$witnesses_involved)

orlando_use_force$white_officers <- 0
orlando_use_force$black_officers <- 0
orlando_use_force$asian_officers <- 0
orlando_use_force$hispanic_officers <- 0
orlando_use_force$unknown_race_officers <- 0



# Makes all the officer race columns
officer_race_split = strsplit(as.character(
  orlando_use_force$officers_race), ";")

officer_race_split <- as.data.frame(do.call(rbind,lapply(
  officer_race_split, `length<-`,
  max(sapply(officer_race_split, length)))))

officer_hispanic_split = strsplit(as.character(
  orlando_use_force$officers_ethnicity), ";")

officer_hispanic_split <- as.data.frame(do.call(rbind,lapply(
  officer_hispanic_split, `length<-`,
  max(sapply(officer_race_split, length)))))

for(i in 1:ncol(officer_race_split)){
  for (n in 1:nrow(orlando_use_force)){

    test_case <- officer_race_split[[i]][n]
    test_case2 <- officer_hispanic_split[[i]][n]

    if (test_case == "W" & !is.na(test_case)) {
      orlando_use_force$white_officers[n] <-
                         orlando_use_force$white_officers[n] + 1
    }
    if (test_case == "B" & !is.na(test_case)) {
      orlando_use_force$black_officers[n] <-
                        orlando_use_force$black_officers[n] + 1
    }
    if (test_case == "A" & !is.na(test_case)) {
      orlando_use_force$asian_officers[n] <-
                       orlando_use_force$asian_officers[n] + 1
    }
    if (test_case == "U" & !is.na(test_case)) {
      orlando_use_force$unknown_race_officers[n] <-
        orlando_use_force$unknown_race_officers[n] + 1
    }

    if (test_case2 == "HI" & !is.na(test_case2)) {
      orlando_use_force$hispanic_officers[n] <-
                    orlando_use_force$hispanic_officers[n] + 1
    }
}
}

# For officer sex. Makes columns indicating how many of each sex
officer_sex_split = strsplit(as.character(
  orlando_use_force$officers_sex), ";")

officer_sex_split <- as.data.frame(do.call(rbind,lapply(
  officer_sex_split, `length<-`,
  max(sapply(officer_sex_split, length)))))

orlando_use_force$male_officers <- 0
orlando_use_force$female_officers <- 0


for(i in 1:ncol(officer_sex_split)){
  for (n in 1:nrow(orlando_use_force)){

    test_case <- officer_sex_split[[i]][n]
    if (test_case == "M" & !is.na(test_case)) {
      orlando_use_force$male_officers[n] <-
        orlando_use_force$male_officers[n] + 1
    }
    if (test_case == "F" & !is.na(test_case)) {
      orlando_use_force$female_officers[n] <-
        orlando_use_force$female_officers[n] + 1
    }
  }
}


# Makes all the offender race columns

orlando_use_force$white_offenders <- 0
orlando_use_force$black_offenders <- 0
orlando_use_force$asian_offenders <- 0
orlando_use_force$hispanic_offenders <- 0
orlando_use_force$unknown_race_offenders <- 0

offender_race_split = strsplit(as.character(
  orlando_use_force$offenders_race), ";")

offender_race_split <- as.data.frame(do.call(rbind,lapply(
  offender_race_split, `length<-`,
  max(sapply(offender_race_split, length)))))

offender_hispanic_split = strsplit(as.character(
  orlando_use_force$offenders_ethnicity), ";")

offender_hispanic_split <- as.data.frame(do.call(rbind,lapply(
  offender_hispanic_split, `length<-`,
  max(sapply(offender_race_split, length)))))

for(i in 1:ncol(offender_race_split)){
  for (n in 1:nrow(orlando_use_force)){

    test_case <- offender_race_split[[i]][n]
    test_case2 <- offender_hispanic_split[[i]][n]

    if (test_case == "W" & !is.na(test_case)) {
      orlando_use_force$white_offenders[n] <-
        orlando_use_force$white_offenders[n] + 1
    }
    if (test_case == "B" & !is.na(test_case)) {
      orlando_use_force$black_offenders[n] <-
        orlando_use_force$black_offenders[n] + 1
    }
    if (test_case == "A" & !is.na(test_case)) {
      orlando_use_force$asian_offenders[n] <-
        orlando_use_force$asian_offenders[n] + 1
    }
    if (test_case == "U" & !is.na(test_case)) {
      orlando_use_force$unknown_race_offenders[n] <-
        orlando_use_force$unknown_race_offenders[n] + 1
    }

    if (test_case2 == "HI" & !is.na(test_case2)) {
      orlando_use_force$hispanic_offenders[n] <-
        orlando_use_force$hispanic_offenders[n] + 1
    }
  }
}

# For offender sex. Makes columns indicating how many of each sex
offender_sex_split = strsplit(as.character(
  orlando_use_force$offenders_sex), ";")

offender_sex_split <- as.data.frame(do.call(rbind,lapply(
  offender_sex_split, `length<-`,
  max(sapply(offender_sex_split, length)))))

orlando_use_force$male_offenders <- 0
orlando_use_force$female_offenders <- 0


for(i in 1:ncol(offender_sex_split)){
  for (n in 1:nrow(orlando_use_force)){

    test_case <- offender_sex_split[[i]][n]
    if (test_case == "M" & !is.na(test_case)) {
      orlando_use_force$male_offenders[n] <-
        orlando_use_force$male_offenders[n] + 1
    }
    if (test_case == "F" & !is.na(test_case)) {
      orlando_use_force$female_offenders[n] <-
        orlando_use_force$female_offenders[n] + 1
    }
  }
}


orlando_use_force$officers_sex <- NULL
orlando_use_force$officers_race <- NULL
orlando_use_force$officers_ethnicity <- NULL
orlando_use_force$offenders_sex <- NULL
orlando_use_force$offenders_race <- NULL
orlando_use_force$offenders_ethnicity <- NULL

message(paste("This dataset does not include Hispanic as a race",
              "(it is considered an ethnicity) so the number of",
              "Hispanic officers is not considered in the total",
              "officer count",
              sep = " "))

orlando_use_force$department_name <- "orlando_police"

return(orlando_use_force)
}

#### Rutland Vermont use of force ####

if (dataset_name == "rutland_use_force") {

rutland_use_force <- fromJSON(paste("https://data.rutlandcitypolice.com",
                                    "/resource/e2x9-2kgt.json",
                                    sep = ""))
rutland_use_force$date <- ymd_hms(rutland_use_force$date_of_incident)
rutland_use_force$date_of_incident <- NULL
rutland_use_force <- defactor(rutland_use_force)

for (i in 1:ncol(rutland_use_force)) {
  rutland_use_force[,i] <- gsub("N/A|^no$", "No", rutland_use_force[,i],
                                ignore.case = TRUE)
}
rutland_use_force$officer_age <- as.numeric(rutland_use_force$officer_age)
rutland_use_force$subject_age <- as.numeric(rutland_use_force$subject_age)

rutland_use_force$department_name <- "rutland_city_police"

return(rutland_use_force)
}
}

