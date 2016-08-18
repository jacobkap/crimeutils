street_cleaner <- function(street){

  street <- gsub("ST$", "street", street, ignore.case = TRUE)
  street <- gsub("CT$", "court", street, ignore.case = TRUE)
  street <- gsub("AV$|ave$", "avenue", street, ignore.case = TRUE)
  street <- gsub("rd$", "road", street, ignore.case = TRUE)
  street <- gsub("bd$|blvd$", "boulevard", street, ignore.case = TRUE)
  street <- gsub("dr$", "drive", street, ignore.case = TRUE)
  street <- gsub("wy$", "way", street, ignore.case = TRUE)
  street <- gsub("^n ", "north ", street, ignore.case = TRUE)
  street <- gsub("^s ", "south ", street, ignore.case = TRUE)
  street <- gsub("^w ", "west ", street, ignore.case = TRUE)
  street <- gsub("^e ", "east ", street, ignore.case = TRUE)
  street <- gsub("[][!#$%()*,.:;<=>@^_`|~.{}-]|\\&", "", street)
  street <- tolower(street)
  return(street)
}

hour_cleaner <- function(hour){
  hour <- gsub("(.*)(..$)", "\\1:\\2", hour)

  for (i in 1:length(hour)){
    if (nchar(hour[i]) < 4) {
      hour[i] <- gsub("(.*)", "00\\1", hour[i])
    }
  }
  return(hour)
}


address_cleaner <- function(address){
  address <- gsub("NA^", "", address, ignore.case = TRUE)
  return(address)
}

race_cleaner <- function(column){
  column <- gsub("^b$", "black", column, ignore.case = TRUE)
  column <- gsub("^w$", "white", column, ignore.case = TRUE)
  column <- gsub("^h$", "hispanic", column, ignore.case = TRUE)
  column <- gsub("^a$", "asian", column, ignore.case = TRUE)
  column <- gsub("^u$", "unknown", column, ignore.case = TRUE)
  return(column)
}

gender_cleaner <- function(column){
  column <- gsub("^m$", "male", column, ignore.case = TRUE)
  column <- gsub("^f$", "female", column, ignore.case = TRUE)
  column <- gsub("^u$", "unknown", column, ignore.case = TRUE)
  column <- tolower(column)
  return(column)
}


binary_cleaner <- function(column){
  column <- gsub("yes", 1, column, ignore.case = TRUE)
  column <- gsub("no", 0, column, ignore.case = TRUE)
  column <- gsub("false", 0, column, ignore.case = TRUE)
  column <- gsub("true", 1, column, ignore.case = TRUE)
    column[column != 0 & column != 1] <- NA
  column <- as.numeric(column)
  return(column)
}


defactor <- function(dataset){
  for (i in 1:ncol(dataset)){
    dataset[,i] <- as.character(dataset[,i])
   }
  return(dataset)
}

