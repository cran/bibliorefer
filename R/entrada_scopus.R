#' Function that generates the test database
#'
#' The example_database function reads a csv file available on the computer system and returns a dataframe as an example of a database to be used by the functions of the bibliorefer package
#'
#'
#' @param path_date is a directory path containing the csv file
#' @param cabec is the header for files in bib format
#' @param quoaspa is the separator for files in bib format
#'
#'
#' @importFrom utils read.delim
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
#' file_db <- system.file("extdata","scopus.bib", package = "bibliorefer")
#' cabec = TRUE
#' quoaspa = ""
#' date_sreference <- entrada_scopus(file_db, cabec, quoaspa)
#' date_sreference
#'
entrada_scopus <- function(path_date, cabec, quoaspa){

  cabe <- cabec
  quoasp <- quoaspa
  #date_package <- read.csv2(path_date, sep = separ)
  date_package <- read.delim(path_date, header = cabe,
                             quote = quoasp)

  return(date_package)
}
