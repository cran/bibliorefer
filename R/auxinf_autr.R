#' Function with information about scientific partnerships
#'
#' This function generates quantitative information about the partnerships of researchers in the development of scientific production on the topic covered. The function returns a list containing 5 dataframes with information about the number of authors who wrote the articles and the number of articles written by each author, both in the general database and in the list of main references.
#'
#' @param input_date is a dataframe with the scientific production database
#' @param input_tam is the length of the dataframe to be generated with the main articles, authors and journals
#'
#' @return The function returns a list containing 5 dataframes with information about the number of authors who wrote the articles and the number of articles written by each author, both in the general database and in the list of main references.
#' @export
#'
#' @references
#'
#' Digitalization as a way forward: A bibliometric analysis of 20 Years of servitization research Zhou, C.,  Song, W. Journal of Cleaner Production, 300, 2021.
#'
#' @examples
#'
#  Call the auxinf_autr function
#' file_db <- system.file("extdata","example_database.csv",package = "bibliorefer")
#' separator <- ","
#' input_date <- example_database(file_db, separator)
#' input_tam <- 100
#' extra_inf <- auxinf_autr(input_date,input_tam)
#' extra_inf
#'
#'

auxinf_autr <- function(input_date,input_tam){

  # Enter the database
  input_date <- input_date
  input_tam <- input_tam

  # Function of ordered information
  font_principal <- function(input_date,input_tam){

    # Enter the database
    input_article <- input_date
    input_autor <- input_date

    # Article citation function
    tabel_principal <- function(input_article, input_tam){

      # Enter the article citation database
      base_ded <- input_article
      # Calculate the size of the database
      tam_art <- length(base_ded$AU)

      #Create the sequence of numbers
      minim_cit <- 1
      maxim_cit <- tam_art
      incm_cit <- 1
      nu_seque <- seq(minim_cit, maxim_cit, incm_cit)

      # Sort the number of citations and positions
      num_cit_ord <- sort(base_ded$TC, decreasing=T)
      nume_ordena <- order(base_ded$TC, decreasing=T)

      # Order the problem variables
      author_ordena <- base_ded$AU[nume_ordena]
      revist_ordena <- base_ded$SO[nume_ordena]
      art_ordena <- base_ded$TI[nume_ordena]
      links_ordena <- base_ded$url[nume_ordena]
      ano_ordena <- base_ded$PY[nume_ordena]
      abstr_ordena <- base_ded$AB[nume_ordena]

      # Create table with frequencies in columns
      tabe_principl <- cbind(ano_ordena, num_cit_ord,
                             nu_seque, art_ordena,
                             author_ordena, abstr_ordena,
                             revist_ordena,links_ordena)
      tabe_principl <- as.data.frame(tabe_principl)

      # Change column names
      colnames(tabe_principl) <- c("Year", "Citations","Position",
                                   "Article","Authors","Abstract",
                                   "Journal", "links")
      #Create the reduced table
      quant_tam <- input_tam
      tabe_principre <- tabe_principl[1:quant_tam,]

      # Show results
      return(tabe_principre)
    }

    # Function of productivity of the authors
    tabel_authors <- function(input_autor){

      # Create the authors database
      base_ded <- input_autor
      base_autor <- cbind(base_ded$AU)

      #Separate the authors
      autor_separ <- strsplit(base_autor, split = ";")
      autor_separ <- cbind(unlist(autor_separ))

      # Calculate the absolute frequency of authors
      fabs_autor <- table(autor_separ)
      fabs_autor <- sort(fabs_autor,decreasing = T)

      # Calculate the relative frequency
      total_autor <- sum(fabs_autor)
      fr_autor <- 100*fabs_autor/total_autor
      fr_autor <- round(fr_autor,digits = 2)

      #Calculate accumulated frequency
      fac_autor <- cumsum(fr_autor)
      fac_autor <- round(fac_autor,digits = 2)

      # Create table with frequencies in columns
      tabefreq_autor <- cbind(fabs_autor, fr_autor, fac_autor)
      tabefreq_autor <- as.data.frame(tabefreq_autor)

      # Create the sequence of positions
      nom_autor <- row.names(fabs_autor)
      tam_autor <- length(nom_autor)

      # Create the sequence of positions
      minim_autor <- 1
      maxim_autor <- tam_autor
      incm_autor <- 1
      num_autor <- seq(minim_autor, maxim_autor, incm_autor)

      # Create the table with the variables
      tabel_autor <- cbind(num_autor, nom_autor,
                           tabefreq_autor$fabs_autor,
                           tabefreq_autor$fr_autor,
                           tabefreq_autor$fac_autor)
      tabel_autor <- as.data.frame(tabel_autor)
      # Change column names
      colnames(tabel_autor) <- c("Position", "Author", "Publications",
                                 "Percentual_publications",
                                 "Acumulated_percentual")
      # Show results
      return(tabel_autor)
    }

    tabel_equipger <- function(input_date){
      #List of separate authors
      geral_autor <- strsplit(input_date$AU, split = ";")
      tam_base <- length(geral_autor)

      #Create variable values
      eq_autge <- numeric(tam_base)
      #Create the loop for length and position counter
      for(i in 1:tam_base){

        eq_autge[i] <- length(geral_autor[[i]])

      }
      #Create variable values
      eq_autge <- as.data.frame(eq_autge)
      colnames(eq_autge) <- "numequip_geral"

      # Calculate the absolute frequency
      fabs_eqautge <- table(eq_autge)
      fabs_eqautge <- as.data.frame(fabs_eqautge)

      # Calculate the relative frequency
      total_eqautge <- sum(fabs_eqautge$Freq)
      fr_eqautge <- 100*fabs_eqautge$Freq/total_eqautge
      fr_eqautge <- round(fr_eqautge, digits = 3)
      fr_eqautge <- as.data.frame(fr_eqautge)

      #Calculate accumulated frequency
      fac_eqautge <- cumsum(fr_eqautge)
      fac_eqautge <- round(fac_eqautge, digits = 3)
      tabel_eqautge <- cbind(fabs_eqautge,fr_eqautge,fac_eqautge)
      tabel_eqautge <- as.data.frame(tabel_eqautge)
      # Change column names
      colnames(tabel_eqautge) <- c("Num_members", "Quantitative",
                                   "Percentual", "Acumulated_percentual")

      return(tabel_eqautge)

    }

    member_equipger <- function(input_date){

      #List of separate authors of general database
      geral_autor <- strsplit(input_date$AU, split = ";")
      tam_base <- length(geral_autor)

      #Create variable values
      membeq_autge <- numeric(tam_base)
      #Create the loop for length and position counter
      for(i in 1:tam_base){

        membeq_autge[i] <- length(geral_autor[[i]])

      }
      #Create variable values
      membeq_autge <- as.data.frame(membeq_autge)
      colnames(membeq_autge) <- "numequip_geral"

      return(membeq_autge)
    }

    # Call internal functions
    tabe_principre <- tabel_principal(input_article,
                                      input_tam)
    tabe_author <- tabel_authors(input_autor)
    tabe_eqipger <- tabel_equipger(input_date)
    members_eqipger <- member_equipger(input_date)

    # Create list with results
    font_princip <- list()
    font_princip <- list(tabe_principre,tabe_author,
                         tabe_eqipger, members_eqipger)
    names(font_princip) <- c("Main_tablered","table_author",
                             "table_equipger",
                             "members_eqipger")

    # Show results
    return(font_princip)

  }

  #Function of auxiliar information authors
  auxinf_aut <- function(font_princip){

    tabe_eqipger <- font_princip[[3]]
    members_eqipger <- font_princip[[4]]

    #Article database
    tabe_principre <- font_princip[[1]]
    colnames(tabe_principre) <- c("year_pub", "citation", "position",
                                  "article","autors","abstract",
                                  "Revists", "links")

    #Author database
    princ_autor <- font_princip[[2]]$Author
    princ_autor <- as.data.frame(princ_autor)
    colnames(princ_autor) <- c("autors")

    #Author publication database
    publications <- font_princip[[2]]$Publications

    # Calculate the absolute frequency
    tafreq1 <- table(publications)
    tafreq1 <-as.data.frame(tafreq1)

    # Calculate the relative frequency
    total_tafreq1 <- sum(tafreq1$Freq)
    fr_tafreq1 <- 100*tafreq1$Freq/total_tafreq1
    fr_tafreq1 <- round(fr_tafreq1,digits = 3)

    #Calculate accumulated frequency
    fac_tafreq1 <- cumsum(fr_tafreq1)
    fac_tafreq1 <- round(fac_tafreq1, digits = 3)
    tabel_articautor <- cbind(tafreq1$publications,
                              tafreq1$Freq,
                              fr_tafreq1,fac_tafreq1)
    tabel_articautor <- as.data.frame(tabel_articautor)
    # Change column names
    colnames(tabel_articautor) <-c("publications","Num_author",
                                   "Percentual",
                                   "Acumulated_percentual")

    #List of separate authors
    lis_autor <- strsplit(tabe_principre$autors, split = ";")
    pautor_separ <- cbind(unlist(lis_autor))

    #Length of databases
    n1 <- length(tabe_principre$position)
    n2 <- length(pautor_separ)

    #Create variable values
    eqp_autor <- numeric(n1)
    #Create the loop for length and position counter
    for(i in 1:n1){

      eqp_autor[i] <- length(lis_autor[[i]])

    }
    #Create variable values
    num_eqp <- as.data.frame(eqp_autor)
    colnames(num_eqp) <- "numequip_autors"

    # Calculate the absolute frequency
    fabs_taeqp <- table(eqp_autor)
    fabs_taeqp <- as.data.frame(fabs_taeqp)

    # Calculate the relative frequency
    total_taeqp <- sum(fabs_taeqp$Freq)
    fr_taeqp <- 100*fabs_taeqp$Freq/total_taeqp
    fr_taeqp <- round(fr_taeqp,digits = 3)

    #Calculate accumulated frequency
    fac_taeqp <- cumsum(fr_taeqp )
    fac_taeqp <- round(fac_taeqp, digits = 3)
    tabel_eqpautor <- cbind(fabs_taeqp$eqp_autor,
                            fabs_taeqp$Freq,
                            fr_taeqp,fac_taeqp)
    tabel_eqpautor <- as.data.frame(tabel_eqpautor)
    # Change column names
    colnames(tabel_eqpautor) <- c("Num_members", "Quantitative",
                                  "Percentual", "Acumulated_percentual")
    # Create list with results
    infor_extr <- list()
    infor_extr <- list(tabel_articautor, tabel_eqpautor,
                       num_eqp, tabe_eqipger,
                       members_eqipger)
    names(infor_extr) <- c("tabel_articautor",
                           "tabel_eqplistred",
                           "nequip_listred",
                           "tabel_eqpger",
                           "nequip_eqpger")

    # Returns the value of the function
    return(infor_extr)

  }

  # Call internal functions
  font_princip <- font_principal(input_date,input_tam)
  inform_extr <- auxinf_aut(font_princip)

  # Returns the value of the function
  return(inform_extr)

}
