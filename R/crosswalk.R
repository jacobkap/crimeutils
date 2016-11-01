setwd("C:/Users/user/Dropbox/R_project/crime/crosswalk")

crosswalk <- read.delim("police_crosswalk.tsv")
save(crosswalk, file = "crosswalk.rda")
