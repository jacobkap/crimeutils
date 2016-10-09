globalVariables("UCR_Offenses_ColNames")

#' Uniform Crime Report Offenses and Clearances Column Name Fixer
#'
#' @param UCR_dataset
#' This is the data.frame containing data from the
#' UCR "Offenses Known and Clearance by Arrest" dataset
#' You may use any years from 1998-2014
#'
#' @return
#' Returns the same data.frame as inputted but with column names corrected
#' as per the UCR "Offenses Known and Clearance by Arrest" codebook for years
#' 1998-2014
#'
#' @export
#'
#' @examples
#' # This is an example data.frame with the same column names
#' # as the real UCR offenses and clearances, dataset.
#' example <- data.frame(V1 = 1:10, V2 = 2:11, V8 = "example")
#' names(example)
#' example <- UCR.OffenseNames(example)
#' names(example)
#'
UCR.OffenseNames <- function(UCR_dataset) {


  for (n in 1:nrow(UCR_Offenses_ColNames)) {
    colnames(UCR_dataset)[which(names(UCR_dataset) ==
                                  UCR_Offenses_ColNames$column_code[n])] <-
      UCR_Offenses_ColNames$column_name[n]
  }


  return(UCR_dataset)

}



#' Fixes column names in LEMAS datasets from 1987 to 2003
#'
#' @param lemas_dataset
#' The LEMAS dataset
#'
#' @param year
#' The year of the LEMAS dataset
#'
#' @return
#' The same data.frame as inputted but with the correct column names
#' @export
#'
#' @examples
#' example <- data.frame(V22 = 1, V30 = 0)
#' lemasNames(example, 1999)
#'
lemasNames <- function(lemas_dataset, year) {

  year <- as.numeric(year)

  stopifnot(year %in% c(1987, 1990, 1993, 1997, 1999, 2000, 2003))

  lemas_codebook <- lemas_codebook[lemas_codebook$year == year,]


  for (n in 1:nrow(lemas_codebook)) {
    colnames(lemas_dataset)[which(names(lemas_dataset) ==
                                    lemas_codebook$column_code[n])] <-
      lemas_codebook$column_name[n]
  }
  return(lemas_dataset)

}


#' Uniform Crime Report Police Employee (LEOKA) Column Name Fixer
#'
#' @param UCR_dataset
#' #' This is the data.frame containing data from the
#' UCR "Police Employee (LEOKA)" dataset
#' You may use any years from 1994-2014
#'
#' @return
#' #' Returns the same data.frame as inputted but with column names corrected
#' as per the UCR "Police Employee (LEOKA)" codebook for years 1994-1997 or years
#' 1998-2014
#'
#' @export
#'
#' @examples
#' # This is an example with 1994-1997 LEOKA column names
#' example <- data.frame(V1 = 1:10, V2 = 2:11, V8 = "example", V6 = 1995)
#' names(example)
#' example <- UCR.LEOKANames(example)
#' names(example)
#'
#' # This is an example with 1998-2014 LEOKA column names
#' example <- data.frame(V1 = 1:10, V2 = 2:11, V8 = "example", V6 = 2000)
#' names(example)
#' example <- UCR.LEOKANames(example)
#' names(example)
UCR.LEOKANames <- function(UCR_dataset) {

  if (length(UCR_dataset$V6 > 0)) {

    if (UCR_dataset$V6[1] %in% 1994:1997) {


      for (n in 1:nrow(LEOKA_ColNames_1994_1997)) {
        colnames(UCR_dataset)[which(names(UCR_dataset) ==
                                      LEOKA_ColNames_1994_1997$column_code[n])] <-
          LEOKA_ColNames_1994_1997$column_name[n]
      }
    }

    else if (UCR_dataset$V6[1] %in% 1998:2014) {

      for (n in 1:nrow(LEOKA_ColNames_1998_2014)) {
        colnames(UCR_dataset)[which(names(UCR_dataset) ==
                                      LEOKA_ColNames_1998_2014$column_code[n])] <-
          LEOKA_ColNames_1998_2014$column_name[n]
      }
    }
  }

  if (length(UCR_dataset$V10 > 0)) {

    if (UCR_dataset$V10[1] %in% 1994:1997) {


      for (n in 1:nrow(LEOKA_ColNames_1994_1997)) {
        colnames(UCR_dataset)[which(names(UCR_dataset) ==
                                      LEOKA_ColNames_1994_1997$column_code[n])] <-
          LEOKA_ColNames_1994_1997$column_name[n]
      }
    }

    else if (UCR_dataset$V10[1] %in% 1998:2014) {

      for (n in 1:nrow(LEOKA_ColNames_1998_2014)) {
        colnames(UCR_dataset)[which(names(UCR_dataset) ==
                                      LEOKA_ColNames_1998_2014$column_code[n])] <-
          LEOKA_ColNames_1998_2014$column_name[n]
      }
    }
  }
  return(UCR_dataset)

}




#' Column name and code for the UCR Offenses dataset
#'
#' A dataset containing the name and code corresponding to columns
#' in the Uniform Crime Report "Offenses Known and Clearance by Arrest"
#' dataset for years 1998-2014
#'
#' @format A data frame with 1448 rows and 2 variables:
#' \describe{
#'   \item{column_name}{The column name for the given column, according
#'   to the codebook.}
#'   \item{column_code}{The code for the given column, according to the codebook.
#'   This will be the column name of the inputted dataset.}
#'   ...
#' }
#' @source \url{http://www.icpsr.umich.edu/icpsrweb/NACJD/studies/36391}
"UCR_Offenses_ColNames"


#' Column name and code for the UCR "Police Employee" (LEOKA)
#'  dataset from 1994-1997
#'
#' A dataset containing the name and code corresponding to columns
#' in the Uniform Crime Report "Police Employee" (LEOKA) dataset for
#' years 1994-1997
#'
#' @format A data frame with 2255 rows and 2 variables:
#' \describe{
#'   \item{column_name}{The column name for the given column, according
#'   to the codebook.}
#'   \item{column_code}{The code for the given column, according to the codebook.
#'   This will be the column name of the inputted dataset.}
#'   ...
#' }
#' @source \url{http://www.icpsr.umich.edu/icpsrweb/NACJD/series/57/studies/9028}
"LEOKA_ColNames_1994_1997"



#' Column name and code for the UCR "Police Employee" (LEOKA)
#'  dataset from 1998-2014
#'
#' A dataset containing the name and code corresponding to columns
#' in the Uniform Crime Report "Police Employee" (LEOKA) dataset for
#' years 1998-2014
#'
#' @format A data frame with 2263 rows and 2 variables:
#' \describe{
#'   \item{column_name}{The column name for the given column, according
#'   to the codebook.}
#'   \item{column_code}{The code for the given column, according to the codebook.
#'   This will be the column name of the inputted dataset.}
#'   ...
#' }
#' @source \url{http://www.icpsr.umich.edu/icpsrweb/NACJD/studies/36395}
"LEOKA_ColNames_1998_2014"
