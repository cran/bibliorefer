#' Function of principal scientifics references
#'
#' This function generates a list, with a size defined by the user, with the main scientific references and also generates quantitative parameters about authors and journals.
#'
#' @param input_date is a dataframe with the scientific production database
#' @param input_tam is the length of the dataframe to be generated with the main articles, authors and journals
#'
#' @return This function returns a list object with two dataframes. The first dataframe, with a length defined by the user, containing the list of articles ordered by the number of citations crossed with the lists of authors and journals ordered by the number of articles published. The second dataframe contains quantitative parameters about authors and journals that are necessary to create the quantitative validation criterion for the list obtained.
#' @export
#'
#' @references
#' A bibliometric analysis of Journal of Higher Education Management (JHEM) from 2007 to 2016. Antia-Obong, S.E., Casselden, B., Pickard, A. Library Philosophy and Practice, 2019.
#'
#' @examples
#' #Call the principal_lister function
#'
#' # Example 1
#' input_tam <- 80
#' file_db <- system.file("extdata","example_database.csv", package = "bibliorefer")
#' separator <- ","
#' input_date <- example_database(file_db, separator)
#' principal_refer <- principal_lister(input_date,input_tam)
#' principal_refer[[1]]
#'
#' # Example 2
#' input_tam <- 40
#' file_db <- system.file("extdata","example_database.csv", package = "bibliorefer")
#' separator <- ","
#' input_date <- example_database(file_db, separator)
#' principal_refer <- principal_lister(input_date,input_tam)
#' principal_refer[[1]]
#'

# Function of the main scientific reference list
principal_lister <- function(input_date,input_tam){

  # Enter the database
  input_date <- input_date
  input_tam <- input_tam

  # Function of ordered information
  font_principal <- function(input_date,input_tam){

    # Enter the database
    input_article <- input_date
    input_autor <- input_date
    input_revist <- input_date

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

    # Function of magazine productivity
    tabel_revists <- function(input_revist){

      # Create the reference database
      base_ded <- input_revist
      base_revist <- base_ded$SO

      # Calculate the absolute frequency of publications
      fabs_revist <- table(base_revist)
      fabs_revist <- sort(fabs_revist, decreasing = T)

      # Calculate the relative frequency
      total_revist <- sum(fabs_revist)
      fr_revist <- 100*fabs_revist/total_revist
      fr_revist <- round(fr_revist,digits = 2)

      #Calculate accumulated frequency
      fac_revist <- cumsum(fr_revist)
      fac_revist <- round(fac_revist, digits = 2)

      #Create a table with frequencies
      tabefreq_revist <- cbind(fabs_revist,fr_revist, fac_revist)
      tabefreq_revist <- as.data.frame(tabefreq_revist)

      # Create the sequence of positions
      tam_revist <- length(fabs_revist)
      mini_revist <- 1
      maxi_revist <- tam_revist
      inc_revist <- 1
      num_revist <- seq(mini_revist, maxi_revist, inc_revist)

      # Create the table with the variables
      nom_revist <- row.names(fabs_revist)
      tabel_revist <- cbind(num_revist, nom_revist,
                            tabefreq_revist$fabs_revist,
                            tabefreq_revist$fr_revist,
                            tabefreq_revist$fac_revist)
      tabel_revist <- as.data.frame(tabel_revist)
      # Change column names
      colnames(tabel_revist) <- c("Position", "Journal", "Publications",
                                  "Percentual_publications",
                                  "Acumulated_percentual")
      # Show results
      return(tabel_revist)
    }

    # Call internal functions
    tabe_principre <- tabel_principal(input_article, input_tam)
    tabe_author <- tabel_authors(input_autor)
    tabe_revist <- tabel_revists(input_revist)

    # Create list with results
    font_princip <- list()
    font_princip <- list(tabe_principre,tabe_author,
                         tabe_revist)
    names(font_princip) <- c("Main_tablered","table_author",
                             "table_journal")
    # Show results
    return(font_princip)
  }

  #Function cross-information of scientific production
  cross_infrefer <- function(font_princip){

    #Article database
    tabe_principre <- font_princip[[1]]
    colnames(tabe_principre) <- c("year_pub", "citation", "position",
                                  "article","autors","abstract",
                                  "Revists", "links")
    #Author database
    princ_autor <- font_princip[[2]]$Author
    princ_autor <- as.data.frame(princ_autor)
    colnames(princ_autor) <- c("autors")

    #Magazine database
    princ_revist <- font_princip[[3]]$Journal
    princ_revist <- as.data.frame(princ_revist)
    colnames(princ_revist) <- c("revist")

    #List of separate authors
    lis_autor <- strsplit(tabe_principre$autors, split = ";")
    pautor_separ <- cbind(unlist(lis_autor))

    #Length of databases
    m <- length(princ_autor$autors)
    n1 <- length(tabe_principre$position)
    n2 <- length(pautor_separ)

    #Position of magazines in relation to articles
    nposi_rev <- numeric(n1)

    #Create the loop
    for(i in 1:n1){

      j <- 0
      repeat {
        j <- j + 1
        if(tabe_principre$Revists[i] == princ_revist$revist[j])
          break
      }
      nposi_rev[i] <- j
    }

    #Create sequence of positions
    minimo <- 1
    maximo <- n2
    incremento <- 1
    posicao <- seq(minimo,maximo,incremento)

    #Create variable size
    tamanho <- numeric(n1)
    posicaocontc<-numeric(n1)
    posicaocont <- numeric(n1)
    artic_posicao <- numeric(n1)
    revist_posicao <- numeric(n1)
    claf_artic <- numeric(n1)
    link_artic <- numeric(n1)
    an_artic <- numeric(n1)
    abstra_artic <- numeric(n1)

    #Create variable values
    tamanho[1] <- length(lis_autor[[1]])
    posicaocontc[1] <- 0
    posicaocont[1] <- tamanho[1]

    #Create the loop for length and position counter
    for(i in 2:n1){

      tamanho[i] <- length(lis_autor[[i]])
      posicaocontc[i] <- posicaocontc[i-1] + tamanho[i-1]
      posicaocont[i] <- posicaocont[i-1] + tamanho[i]

    }

    #Create variable values
    #Create the loop for variable values
    for(i in 1:n1){

      artic_posicao[posicaocont[i]] <- tabe_principre$article[i]
      revist_posicao[posicaocont[i]] <- tabe_principre$Revists[i]
      claf_artic[posicaocont[i]] <- tabe_principre$position[i]
      link_artic[posicaocont[i]] <- tabe_principre$links[i]
      an_artic[posicaocont[i]] <- tabe_principre$year_pub[i]
      abstra_artic[posicaocont[i]] <- tabe_principre$abstract[i]

      }

    #Create the length of variables
    articl <- numeric(n1)
    revistl <- numeric(n1)
    nposi_revist <- numeric(n1)
    cla_articl <- numeric(n1)
    linkl_articl <-numeric(n1)
    ano_articl <- numeric(n1)
    abstrac_articl <- numeric(n1)
    num_autorl <- numeric(n2)

    #Create variable values
    #Create the loop
    for(i in 1:n1){

      j <- 0

      repeat {
        j <- j + 1
        contador <- j + posicaocontc[i]

        if(posicao[contador] != posicaocont[i]){

          articl[contador] <- ""
          revistl[contador] <- ""
          nposi_revist[contador] <- ""
          cla_articl[contador] <- ""
          linkl_articl[contador] <- ""
          ano_articl[contador] <- ""
          abstrac_articl[contador] <- ""
          num_autorl[contador] <- ""

        }else{

          articl[contador] <- artic_posicao[posicaocont[i]]
          revistl[contador] <- revist_posicao[posicaocont[i]]
          nposi_revist[contador] <- nposi_rev[i]
          cla_articl[contador] <- claf_artic[posicaocont[i]]
          linkl_articl[contador] <- link_artic[posicaocont[i]]
          ano_articl[contador] <- an_artic[posicaocont[i]]
          abstrac_articl[contador] <- abstra_artic[posicaocont[i]]
          num_autorl[contador] <- tamanho[i]

        }

        if (posicao[contador] == posicaocont[i])
          break()
      }

    }

    # Create authors variable length
    nposi_autor <- numeric(n2)

    # Position of authors in relation to articles
    #Create the loop
    for(i in 1:n2){
      j <- 0

      repeat {
        j <- j + 1

        if(pautor_separ[i] == princ_autor$autors[j])
          break
      }
      nposi_autor[i] <- j
    }

    # Create the matrix with the variables
    princi_listr <- cbind(ano_articl,cla_articl,articl,linkl_articl,
                          abstrac_articl,
                          num_autorl,pautor_separ,
                          nposi_autor,revistl,nposi_revist)
    princi_listr <- as.data.frame(princi_listr)
    # Name the matrix columns
    colnames(princi_listr) <- c("Year","Position of article","Article",
                                "Link", "Abstract","Number of authors",
                                "Authors","Productive position of author",
                                "Journal","Productive position of journal")

    # Returns the value of the function
    return(princi_listr)

  }

  # Call the source and main list functions
  font_princip <- font_principal(input_date,input_tam)
  cross_lister <- cross_infrefer(font_princip)

  #Author database
  sequenc_autor <- font_princip[[2]]$Position
  sequenc_autor <- cbind(as.numeric(sequenc_autor))
  mposautor <- max(sequenc_autor)

  #Journal database
  sequenc_revist <- font_princip[[3]]$Position
  sequenc_revist <- cbind(as.numeric(sequenc_revist))
  mposrevist <- max(sequenc_revist)

  #Final productive position of authors and journals
  maxi_positions <- cbind(mposautor, mposrevist)
  maxi_positions <- as.data.frame(maxi_positions)

  #Create the list with the results
  princip_lister <- list()
  princip_lister <- list(cross_lister, maxi_positions)
  names(princip_lister) <- c("princ_lister","maxi_positions")

  # Return the result
  return(princip_lister)

}
