# This data will read and clean the FBI's UCR data from 2015
setwd("C:/Users/user/Dropbox/R_project/crime/UCR_2015")

offenses_2015 <- readLines("offenses2015.txt")
# Keeps only rows with agency info, and crimes/clearances countys
offenses_2015 <- offenses_2015[grep("12 mo tot|12015|tot clear",
                                    offenses_2015, ignore.case = TRUE)]
# Make into dataframe
offenses_2015 <- data.frame(offenses_2015)
names(offenses_2015) <- "temp" # This column name is easier to
                               # use than the default
offenses_2015$temp <- as.character(offenses_2015$temp)



# Grabs the information about the agency from row 1/5
# state, agency name, ORI, population of jurisdiction
offenses_2015_final <- data.frame(state = "", agency = "",
                                  ORI = "", population = "",
                                  stringsAsFactors = FALSE)
for (i in seq(1, nrow(offenses_2015), 5)) {

  row_to_check <- offenses_2015$temp[i]
  row_to_check <- gsub("DOC, OIG.|DOC,OIG.|:|\\.|,|& ", "", row_to_check)
  row_to_check <- gsub("([A-Z]) ([A-Z])",
                       "\\1_\\2", row_to_check)
  row_to_check <- gsub("([A-Z]) ([A-Z])",
                       "\\1_\\2", row_to_check)
  row_to_check <- gsub("\\s+", " ", row_to_check)

  storage <- strsplit(row_to_check,
                      split = " ")
  ori <- grep("^.......$", storage[[1]], value = TRUE)
  ori <- ori[ori != storage[[1]][3]]
  ori <- ori[!grepl("_", ori)]
  temp <- data.frame(state = storage[[1]][2],
                     agency = storage[[1]][3],
                     ORI = ori[grep("[A-Z]{2,7}[0-9]{0,5}",
                                             ori)],
                     population = storage[[1]][grep("^pop$", storage[[1]],
                                                    ignore.case = TRUE) + 1])
  offenses_2015_final <- rbind(offenses_2015_final, temp)
}
offenses_2015_final <- offenses_2015_final[-1,]
# Checking that it worked
for (i in sample(1:nrow(offenses_2015_final), 1000)) {
  Sys.sleep(0.5)
  print(offenses_2015_final[i,])
}

# This grabs info on the yearly count of crimes for:
# violent crime, murder, rape, robbery, agg assault,
# simple assault, property crime, burglary, larcent,
# motor vehicle theft, arson

crimes1 <- data.frame(violent_crime = "",
                      murder = "",
                      rape = "",
                      robbery = "",
                      aggravated_assault = "",
                      simple_assault = "",
                      property_crime = "",
                      burglary = "",
                      larceny = "",
                      motor_vehicle_theft = "",
                      arson = "",
                      stringsAsFactors = FALSE)
for (i in seq(2, nrow(offenses_2015), 5)) {
  row_to_check <- offenses_2015$temp[i]
  for (n in c(21, 28, 36, 44, 54, 64, 74, 83, 93, 102, 120)) {
    if (substr(row_to_check, n, n) %in% c(" ", "")) {
      substr(row_to_check, n, n) <- "0"
    }
  }
  row_to_check <- gsub("\\s+", " ", row_to_check)
  storage <- strsplit(row_to_check,
                      split = " ")

  temp <- data.frame(violent_crime = storage[[1]][5],
                        murder = storage[[1]][6],
                        rape = storage[[1]][7],
                        robbery = storage[[1]][8],
                        aggravated_assault = storage[[1]][9],
                        simple_assault = storage[[1]][10],
                        property_crime = storage[[1]][11],
                        burglary = storage[[1]][12],
                        larceny = storage[[1]][13],
                        motor_vehicle_theft = storage[[1]][14],
                        arson = storage[[1]][15])
  crimes1 <- rbind(crimes1, temp)

}
crimes1 <- crimes1[-1,]



# This grabs info on the yearly count of crimes for:
# forcible rape (completed rape), attempted rape
# gun robbery, knife robbery, other weapon robbery,
# unarmed robbery, gun assault, knife assault,
# other weapon assault, unarmed assault,
# forcible entry, nonforcible entry,
# attempted burglary, auto theft, truckbus theft,
# other vehicle theft

crimes2 <- data.frame(forcible_rape = "",
                      attempted_rape = "",
                      gun_robbery = "",
                      knife_robbery = "",
                      other_weapon_robbery = "",
                      strongarm_robbery = "",
                      gun_assault = "",
                      knife_assault = "",
                      other_weapon_assault = "",
                      unarmed_assault = "",
                      forcible_entry = "",
                      nonforcible_entry = "",
                      attempted_burglary = "",
                      auto_theft = "",
                      truckbus_theft = "",
                      other_vehicle_theft = "",
                      stringsAsFactors = FALSE)
for (i in seq(4, nrow(offenses_2015), 5)) {
  row_to_check <- offenses_2015$temp[i]
  for (n in c(19, 27, 34, 41, 49, 57, 64, 71, 79,
              87, 94, 103, 111, 118, 125, 133)) {
    if (substr(row_to_check, n, n) %in% c(" ", "")) {
      substr(row_to_check, n, n) <- "0"
    }
  }
  row_to_check <- gsub("\\s+", " ", row_to_check)
  storage <- strsplit(row_to_check,
                      split = " ")

  temp <- data.frame(forcible_rape = storage[[1]][5],
                        attempted_rape = storage[[1]][6],
                        gun_robbery = storage[[1]][7],
                        knife_robbery = storage[[1]][8],
                        other_weapon_robbery = storage[[1]][9],
                        strongarm_robbery = storage[[1]][10],
                        gun_assault = storage[[1]][11],
                        knife_assault = storage[[1]][12],
                        other_weapon_assault = storage[[1]][13],
                        unarmed_assault = storage[[1]][14],
                        forcible_entry = storage[[1]][15],
                        nonforcible_entry = storage[[1]][16],
                        attempted_burglary = storage[[1]][17],
                        auto_theft = storage[[1]][18],
                        truckbus_theft = storage[[1]][19],
                        other_vehicle_theft = storage[[1]][20],
                        stringsAsFactors = FALSE)
  crimes2 <- rbind(crimes2, temp)

}
crimes2 <- crimes2[-1,]
