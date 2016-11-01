lemas_codebook <- function(year) {
  setwd("C:/Users/user/Dropbox/R_project/crime/lemas_codebooks")
  library(tm)
  Rpdf <- readPDF(control = list(text = "-layout"))
  # Read the PDF
  dataset <- Corpus(URISource(paste("lemas_", year, ".pdf", sep = "")),
                    readerControl = list(reader = Rpdf))

  # Turn values into a data.frame
  dataset <- data.frame(dataset[[1]]$content)

  # Make into character
  dataset[,1] <- as.character(dataset[,1])
  # Gets rid of empty rows
  dataset[dataset[,1] == "",] <- NA
  dataset <-
    data.frame(dataset[
      !is.na(dataset[,1]),])
  # Keeps only rows with variable name and code
  dataset <-
    data.frame(dataset[grep("v[0-9]",
                            dataset[,1],
                            ignore.case = TRUE),])
  dataset[,1] <- as.character(dataset[,1])
  dataset[,1] <- gsub(" {2,}", " ",
                      dataset[,1])
  dataset[,1] <- gsub("^ *(.*)", "\\1",
                      dataset[,1])
  codes <- data.frame(gsub(" .*", "", dataset[,1]))


  # Remove codes from dataset
  codes[,1] <- as.character(codes[,1])
  for (i in 1:nrow(dataset)) {
    dataset[i, 1] <- gsub(codes[i, 1], "",
                          dataset[i, 1])
  }


  # Make all lowercase
  dataset[,1] <- tolower(dataset[,1])
  dataset[,1] <- gsub(" (.*)", "\\1",
                      dataset[,1])
  dataset[,1] <- gsub(":|-|/|#", " ",
                      dataset[,1])
  dataset[,1] <- gsub(" ", "_",
                      dataset[,1])
  dataset[,1] <- gsub("__|___", "_",
                      dataset[,1])
  dataset[,1] <- gsub("^q._|^q..._|^q.._|^q.", "",
                      dataset[,1])
  dataset[,1] <- gsub("^_|,|_etc.$|\\.", "",
                      dataset[,1])
  dataset[,1] <- gsub("_[0-9]*$", "",
                      dataset[,1])


  dataset$codes <- codes[,1]
  names(dataset)[1] <- "column_name"
  names(dataset)[2] <- "column_code"

  dataset$year <- year

  return(dataset)
}
