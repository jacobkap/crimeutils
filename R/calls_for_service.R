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

baltimore_service$department_name <- "baltimore_police"

return(baltimore_service)
}

#### Bloomington indiana calls for service ####

  if (dataset_name == "bloomington_service"){

bloomginton_service1 <- read_csv(url(paste("https://data.bloomington.",
                                "in.gov/dataset/4451a5ba-3f57-4291-814e",
                                "-295964cedea0/resource/27b22f66-b3bd-4a",
                                "1b-98c2-e751ce3bb91e/download/2016-first",
                                "-quarter-calls-for-service.csv",
                                sep = "")))

bloomington_service2 <- read_csv(url(paste("https://data.bloomington.in.gov/",
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

bloomington_service$department_name <- "bloomington_police"

return(bloomington_service)
}

#### Burlington vermont calls for service ####


#### Cincinnati Ohio calls for service ####
  if (dataset_name == "cincinnati_service"){
    cincinnati_service <- fromJSON("https://data.cincinnati-oh.gov/resource/2fwx-vbif.json")

    cincinnati_service$agency <- NULL
    cincinnati_service$block_begin <- NULL
    cincinnati_service$block_end <- NULL
    cincinnati_service$city <- NULL
    cincinnati_service$closed_time_incident <- NULL
    cincinnati_service$phone_pickup_time <- NULL
    cincinnati_service$street_name <- street_cleaner(cincinnati_service$street_name)
    cincinnati_service$street_name <- address_cleaner(cincinnati_service$street_name)


    cincinnati_service$arrival_tume_primary_unit <- ymd_hms(
                           cincinnati_service$arrival_tume_primary_unit)
    cincinnati_service$create_time_incident <- ymd_hms(
                           cincinnati_service$create_time_incident)
    cincinnati_service$dispatch_time_primary_unit <- ymd_hms(
                           cincinnati_service$dispatch_time_primary_unit)

    cincinnati_service$minutes_from_call_to_dispatch <- difftime(
                            cincinnati_service$dispatch_time_primary_unit,
                            cincinnati_service$create_time_incident,
                            units = "mins")

    cincinnati_service$minutes_from_call_to_arrival <- difftime(
                            cincinnati_service$arrival_tume_primary_unit,
                            cincinnati_service$create_time_incident,
                            units = "mins")

    cincinnati_service$minutes_from_dispatch_to_arrival <- difftime(
                            cincinnati_service$arrival_tume_primary_unit,
                            cincinnati_service$dispatch_time_primary_unit,
                            units = "mins")


    names(cincinnati_service) <- gsub("tume", "time", names(cincinnati_service))
    names(cincinnati_service) <- gsub("event_", "incident_", names(cincinnati_service))

    cincinnati_service$department_name <- "cincinnati_police"

    return(cincinnati_service)
    }

  #### Detroit Michigan calls for service

  if (dataset_name == "detroit_service"){

    detroit_service <- fromJSON(paste("https://data.detroitmi.gov/",
                                      "resource/4m7k-f8s3.json",
                                      sep = ""))


    detroit_service$`:@computed_region_47m5_mdaf` <- NULL
    detroit_service$`:@computed_region_5esb_gjfg` <- NULL
    detroit_service$`:@computed_region_d6uh_frh4` <- NULL
    detroit_service$`:@computed_region_gscg_8e47` <- NULL
    detroit_service$`:@computed_region_rzav_7efk` <- NULL
    detroit_service$`:@computed_region_y6sr_nt2p` <- NULL

    detroit_service$location$coordinates <- gsub("^.......$", NA,
                                     detroit_service$location$coordinates)
    detroit_service$latitude <- gsub(".*, (.*).", "\\1",
                                      detroit_service$location$coordinates)

    detroit_service$location <- NULL

    }

}
