#' Function that generates the test database
#'
#' The example_database function reads a csv file available on the computer system and returns a dataframe as an example of a database to be used by the functions of the bibliorefer package
#'
#'
#' @param path_date is a directory path containing the csv file
#' @param separator is the separator for files in csv format
#'
#' @importFrom utils read.csv2
#' @return This function return is a dataframe with database
#' @export
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier
#'
#' @examples
#'
#' #Call the example_database function
#'
#' file_db <- system.file("extdata","example_database.csv", package = "bibliorefer")
#' separator <- ","
#' date_sreference <- example_database(file_db, separator)
#' date_sreference
#'
example_database <- function(path_date, separator){

  separ <- separator
  date_package <- read.csv2(path_date, sep = separ)

  return(date_package)
}
