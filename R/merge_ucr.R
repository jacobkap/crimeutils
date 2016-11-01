library(foreign)
library(readstata13)
library(reshape2)
library(plyr)
library(crime)
# offenses <- merge_offenses()
# historical_offenses <- historical_UCR_offenses_merger()
# historical_offenses <- historical_offenses[historical_offenses$year < 1998,]
# ucr_offenses <- rbind.fill(offenses, historical_offenses)
# save(ucr_offenses, file = "ucr_offenses.rda")

merge_offenses <- function() {
  setwd(paste("C:/Users/user/Dropbox/Consent Decrees/",
              "consentDecree/UCR_data/clearance_by_arrest",
              sep = ""))

  offense_1998 <- read.dta("1998_clearance_by_arrest.dta")
  offense_1998 <- offenses_cleaner(offense_1998)

  offense_1999 <- read.dta("1999_clearance_by_arrest.dta")
  offense_1999 <- offenses_cleaner(offense_1999)

  offense_2000 <- read.dta("2000_clearance_by_arrest.dta")
  offense_2000 <- offenses_cleaner(offense_2000)

  offense_2001 <- read.dta("2001_clearance_by_arrest.dta")
  offense_2001 <- offenses_cleaner(offense_2001)

  offense_2002 <- read.dta("2002_clearance_by_arrest.dta")
  offense_2002 <- offenses_cleaner(offense_2002)

  offense_2003 <- read.dta("2003_clearance_by_arrest.dta")
  offense_2003 <- offenses_cleaner(offense_2003)

  offense_2004 <- read.dta("2004_clearance_by_arrest.dta")
  offense_2004 <- offenses_cleaner(offense_2004)

  offense_2005 <- read.dta("2005_clearance_by_arrest.dta")
  offense_2005 <- offenses_cleaner(offense_2005)

  offense_2006 <- read.dta("2006_clearance_by_arrest.dta")
  offense_2006 <- offenses_cleaner(offense_2006)

  offense_2007 <- read.dta("2007_clearance_by_arrest.dta")
  offense_2007 <- offenses_cleaner(offense_2007)

  offense_2008 <- read.dta("2008_clearance_by_arrest.dta")
  offense_2008 <- offenses_cleaner(offense_2008)

  offense_2009 <- read.dta("2009_clearance_by_arrest.dta")
  offense_2009 <- offenses_cleaner(offense_2009)

  offense_2010 <- read.dta("2010_clearance_by_arrest.dta")
  offense_2010 <- offenses_cleaner(offense_2010)

  offense_2011 <- read.dta("2011_clearance_by_arrest.dta")
  offense_2011 <- offenses_cleaner(offense_2011)

  offense_2012 <- read.dta13("2012_clearance_by_arrest.dta")
  offense_2012 <- offenses_cleaner(offense_2012)

  offense_2013 <- read.dta("2013_clearance_by_arrest.dta")
  offense_2013 <- offenses_cleaner(offense_2013)

  offense_2014 <- read.dta("2014_clearance_by_arrest.dta")
  offense_2014 <- offenses_cleaner(offense_2014)

  offenses <- rbind(offense_1998, offense_1999, offense_2000,
                    offense_2001, offense_2002,
                    offense_2003, offense_2004, offense_2005,
                    offense_2006, offense_2007, offense_2008,
                    offense_2009, offense_2010, offense_2011,
                    offense_2012, offense_2013, offense_2014)
  names(offenses)[1:2] <- c("state", "ORI")
  return(offenses)

}

offenses_cleaner <- function(dataset) {
  dataset <- UCR.OffenseNames(dataset)
  dataset$CASEID <- NULL
  dataset$all_months_reported <- 0
  dataset$all_months_reported[dataset$number_of_months_reported ==
                                "Dec last reported"] <- 1

  # Make yearly total columns - actual number of offenses
  # The temp part of the name is just to make it easier
  # to grep the columns for my subsetted data. It will be
  # removed from the name
  dataset$temp_motor_vehicle_theft <- rowSums(dataset[,
                                                      grep("act__vhc_theft", names(dataset))]) # Vehicle theft
  dataset$temp_murder <- rowSums(dataset[,
                                         grep("act_num_murder", names(dataset))]) # Murder
  dataset$temp_manslaughter <- rowSums(dataset[,
                                               grep("act_num_manslghtr", names(dataset))]) # Manslaughter
  dataset$temp_rape <- rowSums(dataset[,
                                       grep("act_num_rape__totl", names(dataset))]) # Rape
  dataset$temp_forcible_rape <- rowSums(dataset[,
                                                grep("num_forc_rape", names(dataset))]) # Forcible Rape
  dataset$temp_attempted_rape <- rowSums(dataset[,
                                                 grep("act_num_atmptd_rap", names(dataset))]) # Attempted Rape
  dataset$temp_robbery <- rowSums(dataset[,
                                          grep("act_num_rob", names(dataset))]) # Robbery
  dataset$temp_gun_robbery <- rowSums(dataset[,
                                              grep("num_gun_rob", names(dataset))]) # Gun Robbery
  dataset$temp_knife_robbery <- rowSums(dataset[,
                                                grep("num_knife_rob", names(dataset))]) # Knife Robbery
  dataset$temp_other_weapon_robbery <- rowSums(dataset[,
                                                       grep("num_oth_wpn_ro", names(dataset))]) # Other Weapon Robbery
  dataset$temp_strongarm_robbery <- rowSums(dataset[,
                                                    grep("num_str", names(dataset))]) # Strongarm Robbery
  dataset$temp_assault <- rowSums(dataset[,
                                          grep("act_num_asslt", names(dataset))]) # Assault
  dataset$temp_gun_assault <- rowSums(dataset[,
                                              grep("num_gun_ass", names(dataset))]) # Gun Assault
  dataset$temp_knife_assault <- rowSums(dataset[,
                                                grep("num_knife_ass", names(dataset))]) # Knife Assault
  dataset$temp_other_weapon_assault <- rowSums(dataset[,
                                                       grep("oth_wpn_ass", names(dataset))]) # Other Weapon Assault
  dataset$temp_unarmed_assault <- rowSums(dataset[,
                                                  grep("act__hndfeet_asl", names(dataset))]) # Hand/feet Assault
  dataset$temp_simple_assault <- rowSums(dataset[,
                                                 grep("act__simple_asslt", names(dataset))]) # Simple Assault
  dataset$temp_aggravated_assault <- dataset$temp_assault -
    dataset$temp_simple_assault # Aggravated Assault
  dataset$temp_burglary <- rowSums(dataset[,
                                           grep("act__burglary__tot", names(dataset))]) # Burglary
  dataset$temp_attempted_burglary <- rowSums(dataset[,
                                                     grep("act__att_burg", names(dataset))]) # Attempted Burglary
  dataset$temp_forcible_entry <- rowSums(dataset[,
                                                 grep("force_entr", names(dataset))]) # Forcible entry
  dataset$temp_nonforcible_entry <- rowSums(dataset[,
                                                    grep("entrno_for", names(dataset))]) # Nonforcible entry
  dataset$temp_larceny <- rowSums(dataset[,
                                          grep("act__larceny__tot", names(dataset))]) # Larceny
  dataset$temp_auto_theft <- rowSums(dataset[,
                                             grep("act__auto", names(dataset))]) # Auto Theft
  dataset$temp_truckbus_theft <- rowSums(dataset[,
                                                 grep("bus", names(dataset))]) # Truck/bus Theft
  dataset$temp_other_vehicle_theft <- rowSums(dataset[,
                                                      grep("act__oth_vhc", names(dataset))]) # Other vehicle Theft
  dataset$temp_all_fields <- rowSums(dataset[,
                                             grep("act__all_fields", names(dataset))]) # All Fields

  # Make yearly total columns - cleared number of offenses (Total cleared count)
  dataset$temp_motor_vehicle_theft_cleared <- rowSums(dataset[,
                                                              grep("tot_clr_vhc", names(dataset))]) # Vehicle theft cleared
  dataset$temp_murder_cleared <- rowSums(dataset[,
                                                 grep("tot_clr_murder", names(dataset))]) # Murder cleared
  dataset$temp_manslaughter_cleared <- rowSums(dataset[,
                                                       grep("tot_clr_mans", names(dataset))]) # Manslaughter cleared
  dataset$temp_rape_cleared <- rowSums(dataset[,
                                               grep("tot_clr_rape", names(dataset))]) # Rape cleared
  dataset$temp_forcible_rape_cleared <- rowSums(dataset[,
                                                        grep("tot_clr_forc_rap", names(dataset))]) # Forcible Rape
  # cleared
  dataset$temp_attempted_rape_cleared <- rowSums(dataset[,
                                                         grep("tot_clr_atmptd_rap", names(dataset))]) # Attempted Rape
  # cleared
  dataset$temp_robbery_cleared <- rowSums(dataset[,
                                                  grep("tot_clr__totl_rob", names(dataset))]) # Robbery cleared
  dataset$temp_gun_robbery_cleared <- rowSums(dataset[,
                                                      grep("tot_clr_gun_rob", names(dataset))]) # Gun Robbery cleared
  dataset$temp_knife_robbery_cleared <- rowSums(dataset[,
                                                        grep("tot_clr_knife_rob", names(dataset))]) # Knife Robbery
  # cleared
  dataset$temp_other_weapon_robbery_cleared <- rowSums(dataset[,
                                                               grep("tot_clr_oth_wpn_ro", names(dataset))]) # Other Weapon
  # Robbery cleared
  dataset$temp_strongarm_robbery_cleared <- rowSums(dataset[,
                                                            grep("tot_clr_str", names(dataset))]) # Strongarm Robbery
  # cleared
  dataset$temp_assault_cleared <- rowSums(dataset[,
                                                  grep("tot_clr_asslt", names(dataset))]) # Assault cleared
  dataset$temp_gun_assault_cleared <- rowSums(dataset[,
                                                      grep("tot_clr_gun_ass", names(dataset))]) # Gun Assault cleared
  dataset$temp_knife_assault_cleared <- rowSums(dataset[,
                                                        grep("tot_clr_knife_ass", names(dataset))]) # Knife Assault cleared
  dataset$temp_other_weapon_assault_cleared <- rowSums(dataset[,
                                                               grep("tot_clr_oth_wpn_asl", names(dataset))]) # Other Weapon Assault
  #cleared
  dataset$temp_unarmed_assault_cleared <- rowSums(dataset[,
                                                          grep("tot_clr_hnd", names(dataset))]) # Hand/feet Assault
  # cleared
  dataset$temp_simple_assault_cleared <- rowSums(dataset[,
                                                         grep("tot_clr_simple", names(dataset))]) # Simple Assault
  # cleared
  dataset$temp_aggravated_assault_cleared <- dataset$temp_assault_cleared -
    dataset$temp_simple_assault_cleared # Aggravated Assault cleared
  dataset$temp_burglary_cleared <- rowSums(dataset[,
                                                   grep("tot_clr_brg", names(dataset))]) # Burglary cleared
  dataset$temp_attempted_burglary_cleared <- rowSums(dataset[,
                                                             grep("tot_clr_att_burg", names(dataset))]) # Attempted Burglary
  #cleared
  dataset$temp_forcible_entry_cleared <- rowSums(dataset[,
                                                         grep("tot_clr_forc_entr", names(dataset))]) # Forcible entry cleared
  dataset$temp_nonforcible_entry_cleared <- rowSums(dataset[,
                                                            grep("tot_clr_entrno", names(dataset))]) # Nonforcible entry cleared
  dataset$temp_larceny_cleared <- rowSums(dataset[,
                                                  grep("tot_clr_larc", names(dataset))]) # Larceny cleared
  dataset$temp_auto_theft_cleared <- rowSums(dataset[,
                                                     grep("tot_clr_auto", names(dataset))]) # Auto Theft cleared
  dataset$temp_truckbus_theft_cleared <- rowSums(dataset[,
                                                         grep("tot_clr_trck", names(dataset))]) # Truck/bus Theft cleared
  dataset$temp_other_vehicle_theft_cleared <- rowSums(dataset[,
                                                              grep("tot_clr_oth_vhc", names(dataset))]) # Other vehicle
  #Theft cleared
  dataset$temp_all_fields_cleared <- rowSums(dataset[,
                                                     grep("tot_clr_all", names(dataset))]) # All Fields cleared


  # Subsets data into only the columns that we want to keep
  columns_to_grab <- c("ori_code", "numeric", "year", "agency_name",
                       "population_1", "mailing_addressline_2",
                       "temp", "all_months")
  dataset <- dataset[, grep(paste(columns_to_grab, collapse = "|"),
                            names(dataset), value = TRUE)]
  names(dataset) <- gsub("^temp_", "", names(dataset))

  return(dataset)

}



historical_UCR_offenses_cleaner <- function(dataset, crime) {
  names(dataset)[1:3] <- c("ORI", "state", "crime")
  dataset$crime <- NULL
  dataset$dname <- NULL
  dataset$fname <- NULL
  dataset$months <- NULL
  names(dataset)[3:ncol(dataset)] <- gsub(".*(....$)", "year_\\1",
                                          names(dataset)[3:ncol(dataset)])

  for (i in 3:ncol(dataset)) {
    dataset[,i][dataset[,i] < 0] <- 0
  }

  dataset <- aggregate(. ~ ORI + state,
                       data = dataset, FUN = sum)

  for (i in 3:ncol(dataset)) {
    dataset[,i] <- round(dataset[,i])
  }

  dataset <- melt(dataset)
  names(dataset)[3] <- "year"
  dataset$year <- gsub("year_", "", dataset$year)
  dataset$year <- as.numeric(as.character(dataset$year))

  dataset$crime <- crime
  dataset <- dcast(dataset, ORI + state + year
                   ~ crime)

  return(dataset)
}


historical_UCR_offenses_merger <- function() {
  setwd(paste("C:/Users/user/Dropbox/Consent Decrees/",
              "consentDecree/historical_UCR_offenses",
              sep = ""))


  aggravated_assault <- read.dta("aggravated_assault.dta")
  aggravated_assault <- historical_UCR_offenses_cleaner(aggravated_assault,
                                                        crime = "aggravated_assault")

  aggravated_assault_gun <- read.dta("aggravated_assault_gun.dta")
  aggravated_assault_gun <- historical_UCR_offenses_cleaner(
    aggravated_assault_gun,
    crime = "gun_assault")

  aggravated_assault_knife <- read.dta("aggravated_assault_knife.dta")
  aggravated_assault_knife <- historical_UCR_offenses_cleaner(
    aggravated_assault_knife,
    crime = "knife_assault")

  aggravated_assault_other <- read.dta("aggravated_assault_other.dta")
  aggravated_assault_other <- historical_UCR_offenses_cleaner(
    aggravated_assault_other,
    crime = "other_weapon_assault")

  aggravated_assault_personal_weapon <-
    read.dta("aggravated_assault_personal_weapon.dta")
  aggravated_assault_personal_weapon <- historical_UCR_offenses_cleaner(
    aggravated_assault_personal_weapon,
    crime = "unarmed_assault")

  burglary_attempted <- read.dta("burglary_attempted.dta")
  burglary_attempted <- historical_UCR_offenses_cleaner(
    burglary_attempted,
    crime = "attempted_burglary")

  burglary_forcible_entry <- read.dta("burglary_forcible_entry.dta")
  burglary_forcible_entry <- historical_UCR_offenses_cleaner(
    burglary_forcible_entry,
    crime = "forcible_entry")

  burglary_total <- read.dta("burglary_total.dta")
  burglary_total <- historical_UCR_offenses_cleaner(
    burglary_total,
    crime = "burglary")

  burglary_unforced_entry <- read.dta("burglary_unforced_entry.dta")
  burglary_unforced_entry <- historical_UCR_offenses_cleaner(
    burglary_unforced_entry,
    crime = "nonforcible_entry")

  larceny <- read.dta("larceny.dta")
  larceny <- historical_UCR_offenses_cleaner(
    larceny,
    crime = "larceny")

  manslaughter <- read.dta("manslaughter.dta")
  manslaughter <- historical_UCR_offenses_cleaner(
    manslaughter,
    crime = "manslaughter")

  murder <- read.dta("murder.dta")
  murder <- historical_UCR_offenses_cleaner(
    murder,
    crime = "murder")

  rape_attempted <- read.dta("rape_attempted.dta")
  rape_attempted <- historical_UCR_offenses_cleaner(
    rape_attempted,
    crime = "attempted_rape")

  rape_forcible <- read.dta("rape_forcible.dta")
  rape_forcible <- historical_UCR_offenses_cleaner(
    rape_forcible,
    crime = "forcible_rape")

  rape_total <- read.dta("rape_total.dta")
  rape_total <- historical_UCR_offenses_cleaner(
    rape_total,
    crime = "rape")

  robbery_gun <- read.dta("robbery_gun.dta")
  robbery_gun <- historical_UCR_offenses_cleaner(
    robbery_gun,
    crime = "gun_robbery")

  robbery_knife <- read.dta("robbery_knife.dta")
  robbery_knife <- historical_UCR_offenses_cleaner(
    robbery_knife,
    crime = "knife_robbery")

  robbery_other_weapon <- read.dta("robbery_other_weapon.dta")
  robbery_other_weapon <- historical_UCR_offenses_cleaner(
    robbery_other_weapon,
    crime = "other_weapon_robbery")

  robbery_personal_weapon <- read.dta("robbery_personal_weapon.dta")
  robbery_personal_weapon <- historical_UCR_offenses_cleaner(
    robbery_personal_weapon,
    crime = "strongarm_robbery")

  robbery_total <- read.dta("robbery_total.dta")
  robbery_total <- historical_UCR_offenses_cleaner(
    robbery_total,
    crime = "robbery")

  simple_assault <- read.dta("simple_assault.dta")
  simple_assault <- historical_UCR_offenses_cleaner(
    simple_assault,
    crime = "simple_assault")

  vehicle_theft_automobile <- read.dta("vehicle_theft_automobile.dta")
  vehicle_theft_automobile <- historical_UCR_offenses_cleaner(
    vehicle_theft_automobile,
    crime = "auto_theft")

  vehicle_theft_other <- read.dta("vehicle_theft_other.dta")
  vehicle_theft_other <- historical_UCR_offenses_cleaner(
    vehicle_theft_other,
    crime = "other_vehicle_theft")

  vehicle_theft_total <- read.dta("vehicle_theft_total.dta")
  vehicle_theft_total <- historical_UCR_offenses_cleaner(
    vehicle_theft_total,
    crime = "motor_vehicle_theft")

  vehicle_theft_truck <- read.dta("vehicle_theft_truck.dta")
  vehicle_theft_truck <- historical_UCR_offenses_cleaner(
    vehicle_theft_truck,
    crime = "truckbus_theft")

  offenses <- merge(aggravated_assault, aggravated_assault_gun,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, aggravated_assault_other,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, aggravated_assault_knife,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, aggravated_assault_personal_weapon,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, burglary_total,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, burglary_unforced_entry,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, burglary_forcible_entry,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, burglary_attempted,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, larceny,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, manslaughter,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, murder,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, rape_total,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, rape_forcible,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, rape_attempted,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, robbery_total,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, robbery_personal_weapon,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, robbery_other_weapon,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, robbery_knife,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, robbery_gun,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, simple_assault,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, vehicle_theft_truck,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, vehicle_theft_total,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, vehicle_theft_other,
                    by = c("ORI", "year", "state"))
  offenses <- merge(offenses, vehicle_theft_automobile,
                    by = c("ORI", "year", "state"))

  offenses$assault <- offenses$aggravated_assault + offenses$simple_assault

  return(offenses)
}
