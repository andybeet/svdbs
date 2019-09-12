#' Extract Length and weight data from svdbs
#'
#'Extract a list of length and weights for speices sampled in nefsc surveys (spring and fall)
#'This data is extracted from svdbs.union_fscs_svbio
#'
#' @param channel an RODBC object (see \code{\link{connect_to_database}})
#' @param species a specific species code or set of codes. Either numeric or character vector. Defaults to "all" species.
#' Numeric codes are converted to VARCHAR2(3 BYTE) when creating the sql statement. Character codes are short character strings.
#' @param sex character vector. Default = "all". options "M" (male), "F" (female), "U" (unsexed)
#'
#' @return A list is returned:
#'
#'   \item{data}{containing the result of the executed \code{$sql} statement}
#'
#'   \item{sql}{containing the sql call}
#'
#'   \item{colNames}{a vector of the table's column names}
#'
#'@section Reference:
#'Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations.
#'Note: species codes (svspp) are stored in the database as VARCHAR2(3 BYTE)
#'
#' @seealso \code{\link{connect_to_database}}
#'
#' @examples
#' \dontrun{
#' # extracts info for cod (73)
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_length_age(channel,species=73)
#'
#' # extracts info for cod ("COD")
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#' get_length_age(channel,"cod") or
#' get_length_age(channel,"co") or
#' get_length_age(channel,"COD")
#'
#'}
#'
#' @export


get_length_weight <- function(channel, year=1994, species="all", sex="all"){

  if ((year == "all") & (species == "all")) stop("Can not pull all species and all years. Too much data!!")

  # create an SQL query to extract all relavent data from tables
  # list of strings to build where clause in sql statement
  whereVec <- list()

  whereVec[[1]] <-  createString(itemName="svspp",species,convertToCharacter=TRUE,numChars=3)
  whereVec[[3]] <-  createStringYear(itemName="year",year,convertToCharacter=TRUE,numChars=4)

  # sex conversion
  if (tolower(sex) == "all") {
    sex <- c(0,1,2)
  } else if (!is.numeric(sex)) {
    sex <- gsub("M",1,sex)
    sex <- gsub("F",2,sex)
    sex <- as.numeric(gsub("U",0,sex))
  }

  whereVec[[4]] <-  paste("sex in (",toString(sex),")")

  # build where clause of SQL statement based on input above
  whereStr <- "where"
  for (item in whereVec) {
    if (is.null(item)) {next}
    if (which(item == whereVec) == length(whereVec)) {
      whereStr <- paste(whereStr,item)
      next
    }
    whereStr <- paste(whereStr,item,"and")
  }



  # eventually user will be able to pass these variables
  sqlStatement <- "select m.cruise6, m.stratum, m.tow, m.station, m.svspp, m.sex, m.indid, m.length, m.indwt, s.season
                    from svdbs.union_fscs_svbio m LEFT JOIN svdbs.svdbs_cruises s
                    ON m.cruise6 = s.cruise6 "

  sqlStatement <- paste(sqlStatement,whereStr)
  # call database
  query <- RODBC::sqlQuery(channel,sqlStatement,errors=TRUE,as.is=TRUE)

  # column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'UNION_FSCS_SVBIO' and owner='SVDBS';"
  colNames <- RODBC::sqlQuery(channel,sqlcolName,errors=TRUE,as.is=TRUE)

  query <- dplyr::as_tibble(query)
  query$SEASON <- as.factor(query$SEASON)
  query$LENGTH <- as.numeric(query$LENGTH)
  query$INDWT <- as.numeric(query$INDWT)
  query$SEX <- as.factor(query$SEX)


  return (list(data =query,sql=sqlStatement, colNames=colNames))
}



