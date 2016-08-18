calls_for_service_cleaner <- function(dataset_name){


#### Baltimore maryland calls for service ####

  if(dataset_name == "baltimore_service"){

baltimore_service <- fromJSON(paste("https://data.baltimorecity.gov/",
                                    "resource/m8g9-abgb.json",
                                    sep = ""))
baltimore_service$call_date <- ymd_hms(baltimore_service$calldatetime)
baltimore_service$calldatetime <- NULL
baltimore_service <- defactor(baltimore_service)

baltimore_service$longitude <- gsub(".*,(.*).",  "\\1",
                                    baltimore_service$location)
baltimore_service$latitude <- gsub(".(.*),.*",  "\\1",
                                    baltimore_service$location)
baltimore_service$location <- NULL

return(baltimore_serivce)
}

#### Bloomington indiana calls for service ####

  if (dataset_name == "bloomington_service"){

bloomginton_service1 <- read.csv(url(paste("https://data.bloomington.",
                                "in.gov/dataset/4451a5ba-3f57-4291-814e",
                                "-295964cedea0/resource/27b22f66-b3bd-4a",
                                "1b-98c2-e751ce3bb91e/download/2016-first",
                                "-quarter-calls-for-service.csv",
                                sep = "")))

bloomington_service2 <- read.csv(url(paste("https://data.bloomington.in.gov/",
                                 "dataset/4451a5ba-3f57-4291-814e-295964cedea",
                                 "0/resource/d4928b37-3547-4e57-ad81-d2d598da6",
                                 "efe/download/2016-second-quarter-calls-for-",
                                 "service.csv",
                                 sep = "")))

bloomington_service <- rbind(bloomginton_service1, bloomington_service2)
rm(bloomginton_service1)
rm(bloomington_service2)

bloomington_service <- defactor(bloomington_service)
names(bloomington_service) <- tolower(names(bloomington_service))
names(bloomington_service) <- gsub("\\.", "_", names(bloomington_service))


bloomington_service$date_time_reported <- paste(
                                          bloomington_service$date_reported,
                                          bloomington_service$time_reported,
                                          sep = " ")
bloomington_service$date_time_reported <- gsub("..$", "",
                          bloomington_service$date_time_reported)
bloomington_service$date_time_reported <- mdy_hm(
                                  bloomington_service$date_time_reported)
bloomington_service$date_reported <- mdy(bloomington_service$date_reported)

names(bloomington_service) <- gsub("report_no_report",
                                   "police_report_made",
                                   names(bloomington_service))

return(bloomington_service)
}

#### Burlington vermont calls for service ####

if (dataset_name == "burlington_service"){

burlington_service <- read.csv(url(paste("https://public.tableau.",
                              "com/vizql/vud/sessions/012144F1B612",
                              "4D8A9A45AA97C7A76B83-0:0/views/81520",
                              "05872074396685_13949870936102779885?",
                              "csv=true&showall=true",
                              sep = "")))

burlington_service <- defactor(burlington_service)

return(burlington_service)
}
}
