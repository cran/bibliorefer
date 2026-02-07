#' Function that generates the test database
#'
#' The example_database function reads a csv file available on the computer system and returns a dataframe as an example of a database to be used by the functions of the bibliorefer package
#'
#'
#' @param path_date is a directory path containing the bib file
#' @param cabec_wos is the header for wos files in bib format
#' @param quoaspa_wos is the separator wos for files in bib format
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
#' file_db <- system.file("extdata","savedrecs.bib", package = "bibliorefer")
#' cabec_wos = TRUE
#' quoaspa_wos = ""
#' date_wosrefer <- entrada_wos(file_db, cabec_wos, quoaspa_wos)
#' date_wosrefer
#'
entrada_wos <- function(path_date, cabec_wos, quoaspa_wos){

  cabe_wos <- cabec_wos
  quoasp_wos <- quoaspa_wos
  date_package <- path_date

  return(date_package)
}

