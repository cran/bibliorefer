#' This function converts the BibTeX format from the Scopus and WoS databases to a CSV spreadsheet.
#'
#' This function converts metadata files from the Scopus and WoS databases in BibTeX format to a CSV spreadsheet. The function takes .bib files from the Scopus and WoS collections as input and returns a dataframe.
#'
#'
#' @param font_scopus is the database that contains the metadata in Bibtex format for articles in the Scopus collection.
#' @param font_wos is the database that contains the metadata in Bibtex format for articles in the WoS collection.
#'
#'
#' @importFrom utils read.delim
#' @return This function returns a dataframe containing the metadata for the articles in the Scopus and WoS collections.
#' @export
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier
#'
#' @examples
#'
#' #Call the junt_scopuswos function
#'
#' file_db <- system.file("extdata","scopus.bib", package = "bibliorefer")
#' cabec = TRUE
#' quoaspa = ""
#' base_scopus <- entrada_scopus(file_db, cabec, quoaspa)
#' base_scopus <- as.data.frame(base_scopus)
#' variavel1 <- base_scopus$V1
#' variavel2 <- base_scopus$V2
#'
#' #Call the convert_biblioscopus function
#' font_scopus <- convert_biblioscopus(variavel1,variavel2)
#'
#' #Call the convert_bibliowos function
#'
#' file_db <- system.file("extdata","savedrecs.bib", package = "bibliorefer")
#' cabec_wos = TRUE
#' quoaspa_wos = ""
#' arquivos <- entrada_wos(file_db, cabec_wos, quoaspa_wos)
#'
#' #Call the convert_bibliowos function
#' font_wos <- convert_bibliowos(arquivos)
#'
#' #Call the junt_scopuswos function
#' font_scopuswos <- junt_scopuswos(font_scopus,font_wos)
#' font_scopuswos
#'
#'
#'
junt_scopuswos <- function(font_scopus,font_wos){

  #Entrada de dados
  base_scopus <- font_scopus
  base_wos <- font_wos

  # Analisar o padrão das linhas
  var_scopus <- base_scopus$doi
  var_wos <- base_wos$doi
  caracvar_scopus <- nchar(var_scopus)
  caracvar_wos <- nchar(var_wos)
  padrao_nulo <- "0"
  naonulo_caracscopus <- which(caracvar_scopus!= padrao_nulo)
  naonulo_caracwos <- which(caracvar_wos != padrao_nulo)

  # Criar as variáveis
  varia_scopus <- var_scopus[naonulo_caracscopus]
  varia_wos <- var_wos[naonulo_caracwos]

  # Calcular o tamanho das variáveis
  tama_variascopus <- length(varia_scopus)
  tama_variawos <- length(varia_wos)

  # Fazer o teste e criar posicoes
  teste_scopus <- varia_scopus %in% varia_wos
  teste_wos <- varia_wos %in% varia_scopus
  padrao_verd <- "TRUE"
  posic_verdscopus <- grep(padrao_verd,teste_scopus)
  posic_verdwos <- grep(padrao_verd,teste_wos)
  tama_repetscopus <- length(posic_verdscopus)
  tama_repetwos <- length(posic_verdwos)

  # Criar as linhas repetidas
  linha_verdscopus <- 1:tama_variascopus
  linha_verdwos <- 1:tama_variawos
  linha_verdscopus[posic_verdscopus] <- posic_verdscopus
  linha_verdwos[posic_verdwos] <- posic_verdwos

  # Determinar o doi repetido
  doi_scopus <- varia_scopus[linha_verdscopus[posic_verdscopus]]
  doi_wos <- varia_wos[linha_verdwos[posic_verdwos]]

  # Criar as linhas do doi repetido na base scopus
  linha_doiexc <- numeric(tama_repetscopus)

  for (j in 1:tama_repetscopus){

    doi_teste <- doi_scopus[j]
    linha_doiex <- which(var_scopus == doi_teste)
    linha_doiexc[j] <- linha_doiex

  }

  # Excluir os artigos repetidos da base Scopus
  base_scopusexc <- base_scopus[-linha_doiexc,]

  # Criar a base conjunta
  base_scopuswos <- rbind(base_scopusexc,base_wos)

  # Criar a base dos repetidos
  scopus_duplicada <- font_scopus[linha_doiexc,]

  # Criar a saida da função
  saida <- list(tama_repetscopus,doi_scopus,
                base_scopuswos,linha_doiexc,
                scopus_duplicada)

  return(saida)

}
