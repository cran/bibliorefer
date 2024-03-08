#' Function of scientific information
#'
#' This function receives a dataframe with scientific production data and returns a list object with information about articles, authors and journals.
#'
#' @param input_date is a dataframe with the scientific production database
#'
#' @return The function return a list object containing three dataframes with ordered lists of articles, authors and journals.
#' @export
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier
#'
#' @examples
#'
#' #Call the tabel_gera function
#' file_db <- system.file("extdata","example_database.csv",package = "bibliorefer")
#' separator <- ","
#' input_date <- example_database(file_db, separator)
#' scientific_inf <- tabel_gera(input_date)
#' scientific_inf
#'
#'
tabel_gera <- function(input_date){

  # Enter the database
  input_article <- input_date
  input_autor <- input_date
  input_revist <- input_date

  # Article citation function
  tabel_articles <- function(input_article){

    #Article database
    base_ded <-input_article

    #Create the sequence of numbers
    tam_art <- length(base_ded$AU)
    minim_cit <- 1
    maxim_cit <- tam_art
    incm_cit <- 1
    nu_seque <- seq(minim_cit, maxim_cit, incm_cit)

    # Order the variables
    nume_ordena <- order(base_ded$TC, decreasing=T)
    art_ordena <- base_ded$TI[nume_ordena]
    ano_ordena <- base_ded$PY[nume_ordena]

    #Calculate relative frequency
    num_cit_ord <- sort(base_ded$TC, decreasing=T)
    total_cit<-sum(num_cit_ord)
    fr_numcit <- 100*num_cit_ord/total_cit
    fr_numcit <- round(fr_numcit,digits = 3)

    #Calculate accumulated frequency
    fac_numcit <- cumsum(fr_numcit)
    fac_numcit <- round(fac_numcit,digits = 3)

    # Create the frequency table of article citations
    tabfreq_cit <- cbind(nu_seque, ano_ordena, art_ordena,
                         num_cit_ord, fr_numcit, fac_numcit)
    tabfreq_cit <- as.data.frame(tabfreq_cit)
    # Name the columns of the frequency table
    colnames(tabfreq_cit) <- c("Position", "Year","Article",
                               "Citations", "Percentual_citations",
                               "Acumulated_percentual")
    # Return article citation results
    return(tabfreq_cit)
  }

  # Function of productivity of the authors
  tabel_authors <- function(input_autor){

    # Author database
    base_ded <- input_autor
    base_autor <- cbind(base_ded$AU)

    #Separate the authors
    autor_separ <- strsplit(base_autor, split = ";")
    autor_separ <- cbind(unlist(autor_separ))

    #Calculate the absolute frequency
    fabs_autor<-table(autor_separ)
    fabs_autor <- sort(fabs_autor,decreasing = T)

    #Calculate relative frequency
    total_autor <- sum(fabs_autor)
    fr_autor <- 100*fabs_autor/total_autor
    fr_autor <- round(fr_autor,digits = 3)

    #Calculates the accumulated frequency
    fac_autor <- cumsum(fr_autor)
    fac_autor <-round(fac_autor,digits = 3)

    # Group frequencies into a matrix
    tabefreq_autor <- cbind(fabs_autor, fr_autor, fac_autor)
    tabefreq_autor <- as.data.frame(tabefreq_autor)

    #Create the sequence of numbers
    nom_autor <- row.names(fabs_autor)
    tam_autor <- length(nom_autor)
    minim_autor <- 1
    maxim_autor <- tam_autor
    incm_autor <- 1
    num_autor <- seq(minim_autor, maxim_autor, incm_autor)

    # Create the author frequency table
    tabel_autor <- cbind(num_autor, nom_autor,
                         tabefreq_autor$fabs_autor,
                         tabefreq_autor$fr_autor,
                         tabefreq_autor$fac_autor)
    tabel_autor <- as.data.frame(tabel_autor)
    # Name the columns of the frequency table
    colnames(tabel_autor) <- c("Position", "Author", "Publications",
                               "Percentual_publications",
                               "Acumulated_percentual")

    # Return results from authors
    return(tabel_autor)
  }

  # Function of magazine productivity
  tabel_revists <- function(input_revist){

    # Magazine database
    base_ded <- input_revist
    base_revist <- base_ded$SO

    # Calculate absolute frequency
    fabs_revist <- table(base_revist)
    fabs_revist <- sort(fabs_revist, decreasing = T)

    #Calculate relative frequency
    total_revist <- sum(fabs_revist)
    fr_revist <- 100*fabs_revist/total_revist
    fr_revist <- round(fr_revist,digits = 3)

    #Calculates the accumulated frequency
    fac_revist <- cumsum(fr_revist)
    fac_revist <- round(fac_revist, digits = 3)

    #Create the magazine frequency table
    tabefreq_revist <- cbind(fabs_revist,fr_revist,
                             fac_revist)
    tabefreq_revist <- as.data.frame(tabefreq_revist)

    #Create the sequence of numbers
    tam_revist <- length(fabs_revist)
    mini_revist <- 1
    maxi_revist <- tam_revist
    inc_revist <- 1
    num_revist <- seq(mini_revist,maxi_revist,inc_revist)

    #Create the magazine frequency table
    nom_revist <- row.names(fabs_revist)
    tabel_revist <- cbind(num_revist, nom_revist,
                          tabefreq_revist$fabs_revist,
                          tabefreq_revist$fr_revist,
                          tabefreq_revist$fac_revist)
    tabel_revist <- as.data.frame(tabel_revist)
    #Name the columns of the frequency table
    colnames(tabel_revist) <- c("Position", "Journal", "Publications",
                                "Percentual_publications",
                                "Acumulated_percentual")
    #Return magazine results
    return(tabel_revist)
  }

  #Call functions
  tabe_artic <- tabel_articles(input_article)
  tabe_author <- tabel_authors(input_autor)
  tabe_revist <- tabel_revists(input_revist)

  #Create the general list with tables
  tabe_ger <- list()
  tabe_ger <- list(tabe_artic,tabe_author,tabe_revist)
  names(tabe_ger) <- c("table_article","table_author",
                       "table_journal")

  # Return overall results
  return(tabe_ger)

}
