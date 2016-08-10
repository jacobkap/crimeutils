##### Austin Texas use of force #####


##### Baltimore Maryland use of force ####
baltimore_use_force <- read.socrata(paste("https://data.baltimorecity.gov/",
                      "Public-Safety/BPD-Officer-Involved-Use-Of-Force/3w4d",
                      "-kckv",
                      sep = ""))

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


#### Bloomington Indiana use of force ####

bloomington_use_force1 <- read.csv(url(paste("https://data.bloomington.",
                        "in.gov/dataset/93c0b51f-f6b6-475e-b2f1-852c4f5c",
                        "6eb7/resource/a3e4c4c3-dc1f-4246-bfe3-41baa85aa4",
                        "66/download/2016-first-quarter-use-of-force.csv",
                        sep = "")),
                        skip = 1)

bloomington_use_force2 <- read.csv(url(paste("https://data.bloomington.in.",
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

