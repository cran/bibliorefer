#' Function that consolidates files in BibTeX format from the WoS database.
#'
#' The function combines the various Bibtex files from the WoS collection into a single file. The function returns a list with the sequence of grouped files and a dataframe with the final Bibtex database.
#'
#'
#' @param arquivos is the database that contains the metadata in Bibtex format for the articles in the WoS collection.
#'
#'
#' @importFrom utils read.delim
#' @return This function returns a dataframe containing the metadata for the articles in the WoS collection.
#' @export
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier
#'
#' @examples
#'
#' #Call the arquiwos_une function
#'
#' file_db <- system.file("extdata","savedrecs.bib", package = "bibliorefer")
#' cabec_wos = TRUE
#' quoaspa_wos = ""
#' arquivos <- entrada_wos(file_db, cabec_wos, quoaspa_wos)
#'
#' #Call the arquiwos_une function
#' base_arquiwos <- arquiwos_une(arquivos)
#' base_arquiwos
#'
#'
#'
arquiwos_une <- function(arquivos){

  # Criar o conjunto de arquivos WoS
  tama_arquivo <- length(arquivos)
  conjunto_arquivos <- c()

  # Criar o laço
  for(j in 1:tama_arquivo){

    conjunto_arquivos[j] <- arquivos[j]

  }

  base_entrada <- list()

  if (tama_arquivo == 1){

    base_entra <- read.delim(conjunto_arquivos[tama_arquivo], header = F, sep= "\t",quote="")
    base_entrada[[tama_arquivo]] <- base_entra
    basededados <- base_entrada[[tama_arquivo]]

  }else{

    # Criar a base de dados da WoS
    base_entra <- read.delim(conjunto_arquivos[1], header = F, sep= "\t",quote="")
    base_entrada[[1]] <- base_entra

    for(i in 2:tama_arquivo){

      base_entra <- read.delim(conjunto_arquivos[i], header = F,
                               sep= "\t",quote="")

      base_entrada[[i]] <- rbind(base_entrada[[i-1]],base_entra)

    }

    basededados <- base_entrada[[tama_arquivo]]

  }

  # Criar a variável

  variavel_wos <- basededados$V1

  if (variavel_wos[1] == "V1"){

    linha_diferen <- which(variavel_wos!= variavel_wos[1])
    variavel1 <- basededados$V1[linha_diferen]

  }else{

    variavel1 <- basededados$V1

  }

  V1 <- variavel1
  base_final <- cbind(V1)
  base_final <- as.data.frame(base_final)
  basededados <- list(base_entrada,base_final)

  return(basededados)
}

