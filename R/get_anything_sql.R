#' Extract anything from SVDBS
#'
#'Extract any information from any database. This function is for people who know sql and understand the database well enough
#'to write their own sql statements. For all others, please use other functions in package
#'
#' @param channel an Object inherited from \link[DBI]{DBIConnection-class}. This object is used to connect
#' to communicate with the database engine. (see \code{\link{connect_to_database}})
#' @param sqlStatement an sql statement
#' @return A list is returned:
#'
#'  - \code{data} containing the result of the executed \code{sqlStatement} and
#'
#'  - \code{sql} containing the \code{sqlStatement} itself
#'
#'  - \code{colNames} a vector of the table's column names
#'
#'
#' There is no default sql statement.
#'
#'@section Warning:
#'You will need to obtain read only privilages to access any of the databases
#'Some of the databases contains many million records. If you try to pull the entire database it is likely you will run out of memory.
#'
#'@section Reference:
#'Use the data dictionary (\url{http://nova.nefsc.noaa.gov/datadict/}) for field name explanations
#'
#' @seealso \code{\link{connect_to_database}}
#'
#' @examples
#' \dontrun{
#'
#' # extracts silver hake (072) in area stratum (01010) for year 2001
#' channel <- connect_to_database(server="name_of_server",uid="individuals_username")
#'
#' sqlStatement <- "select cruise6, stratum, tow, station, length, indwt,sex from svdbs.union_fscs_svbio
#'  where  (svspp in 72)  and (stratum in 01010) and (cruise6 like '2001%');"
#'
#' get_anything_sql(channel,sqlStatement)
#'}
#'
#' @export

get_anything_sql <- function(channel,sqlStatement) {

  if (grepl("\\*",sqlStatement)) {
    warning ("Use of wild card in sql statement - You may be pulling too much data, your computer memory could get maxed out!!! ")

    #query <- NULL
    query <- DBI::dbGetQuery(channel,sqlStatement)

  } else {
    query <- DBI::dbGetQuery(channel,sqlStatement)
  }

  # get column names
  sqlcolName <- "select COLUMN_NAME from ALL_TAB_COLUMNS where TABLE_NAME = 'MV_CF_LANDINGS' and owner='STOCKEFF';"
  colNames <- DBI::dbGetQuery(channel,sqlcolName)

  return (list(data=dplyr::as_tibble(query), sql = sqlStatement,colNames=colNames))

}


