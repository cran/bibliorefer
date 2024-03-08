#' Function of annual scientific production
#'
#' This function shows the evolution of scientific production. The function receives a dataframe with scientific production data and returns a dataframe with the dates and number of articles published on each date
#'
#' @param input_date is a dataframe with the scientific production database
#'
#'
#' @return The function return a dataframe with the dates and the number of articles published in each of them
#' @export
#'
#' @references
#'
#' Evolution of number of citations per article in Materials Science: possible causes and effect on the impact factor of journals. Ariza-Guerrero, A.M, Blázquez, J.S. Scientometrics, 128(12), pp. 6589–6609. 2023.
#'
#' @examples
#'
#' #Call the produc_cientific function
#' file_db <- system.file("extdata","example_database.csv", package = "bibliorefer")
#' separator <- ","
#' input_date <- example_database(file_db, separator)
#' prod_ct <- produc_cientific(input_date)
#' prod_ct
#'
#'

produc_cientific <- function(input_date){

  # Create the database
  base_ded <- input_date
  ano_prod <- base_ded$PY

  # Calculate the absolute frequency
  fabs <- table(ano_prod)

  #Calculate relative frequency
  total <- sum(fabs)
  fr <- 100*fabs/total
  fr <- round(fr,digits = 3)

  #Calculate accumulated frequency
  fac <- cumsum(fr)

  # Create the matrix with frequencies
  anosr <- row.names(fabs)
  produc <- cbind(anosr,fabs,fr,fac)
  produc <- as.data.frame(produc)
  produc$fabs <- as.numeric(produc$fabs)
  produc$anosr <- as.numeric(produc$anosr)

  # Create the date sequence
  mini_prod <- as.numeric(min(produc$anosr))
  maxi_prod <- as.numeric(max(produc$anosr))
  incr_prod <- 1
  datas_an <- seq(mini_prod, maxi_prod,incr_prod)

  # Calculate the size of variables
  n <- length(produc$fabs)
  n1 <- maxi_prod - mini_prod + 1

  # Create variables
  contadores <- numeric(n)
  dife_anos <- numeric(n)
  produ_cientif <- numeric(n1)

  # Start variables
  contador1 <- 1
  contador2 <- 1
  produ_cientif[contador1] <- produc$fabs[1]
  dife_anos[1] <- 0

  # Create the outer loop
  for(i in 2:n){

    dife_anos[i] <- produc$anosr[i] - produc$anosr[i-1]

    # Create the inner loop
    j <- 0
    repeat {

      j <- j + 1
      contador1 <- j
      contador2 <- contador1 - 1

      if (datas_an[contador1] == produc$anosr[i])
        break()
    }

    if(dife_anos[i] > 1){

      produ_cientif[contador1] <- produc$fabs[i]
      produ_cientif[contador2] <- 0

    }else{

      produ_cientif[contador1] <- produc$fabs[i]
    }

    contadores[i] <- contador1
  }

  # Create the column matrix with production
  produ_cientif <- cbind(produ_cientif)

  #Calculate relative frequency
  total <- sum(produ_cientif)
  frc <- 100*produ_cientif/total
  frc <- round(frc,digits = 3)

  #Calculate accumulated frequency
  facc <- cumsum(frc)

  # Create the frequency table
  produ_cientificr <- cbind(datas_an,produ_cientif,frc,facc)
  produ_cientificr <- as.data.frame(produ_cientificr)

  # Rename columns
  colnames(produ_cientificr) <- c("Year","Number of articles",
                                  "Percentage",
                                  "Accumulated percentage")

  # Show scientific production
  return(produ_cientificr)
}
