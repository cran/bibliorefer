#' Function that converts the BibTeX format from the Scopus database to a CSV spreadsheet.
#'
#' The function converts the Scopus database metadata file from Bibtex format to a CSV spreadsheet. The function's input is a file with the .bib extension. The function returns a dataframe.
#'
#'
#' @param variavel1 is the database that contains the input and output keys for metadata in Bibtex format for articles in the Scopus collection.
#' @param variavel2 is the database that contains the metadata in Bibtex format for articles in the Scopus collection.
#'
#'
#' @importFrom utils read.delim
#' @return This function returns a dataframe containing the metadata for the articles in the Scopus collection.
#' @export
#'
#' @references
#' Aria, M. & Cuccurullo, C. (2017) bibliometrix: An R-tool for comprehensive science mapping analysis, Journal of Informetrics, 11(4), pp 959-975, Elsevier
#'
#' @examples
#'
#' #Call the convert_biblioscopus function
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
#' font_scopus
#'
#'
#'
convert_biblioscopus <- function(variavel1,variavel2){

  # Criar as variáveis
  variavel1 <- variavel1
  variavel2 <- variavel2

  # Calcular a quantidade de linhas da tabela
  padrao_article <- "@ARTICLE"
  linhas_article <- grep(padrao_article,variavel1)
  num_linhasarticle <- length(linhas_article)

  padrao_chave <- "}"
  linhas_chave <- grep(padrao_chave,variavel1)
  linhas_chavefim <- max(linhas_chave)
  num_linhaschave <- length(linhas_chave)

  # Criar a largura das faixas
  sequencia1 <- 1:(num_linhasarticle-1)
  sequencia2 <- 2:num_linhasarticle
  larg_faixa <- linhas_article[sequencia2]-linhas_article[sequencia1]
  larg_faixasfi <- linhas_chave[num_linhaschave] - linhas_article[num_linhaschave]
  larg_faixas <- c(larg_faixa,larg_faixasfi)

  ###
  # Encontrar as linhas dos autores
  padrao_aut <- "author ="
  padrao_titu <- "title ="
  padrao_ano <- "year ="
  padrao_jorn <- "journal ="
  padrao_vol <- "volume ="
  padrao_num <- "number ="
  padrao_pag <- "pages ="
  padrao_doi <- "doi ="
  padrao_abs <- "abstract ="
  padrao_afili <- "affiliations ="
  padrao_issn <- "issn = "
  padrao_endemail <- "correspondence_address = "
  padrao_nota <- "note = "
  padrao_pachave <- "author_keywords = "

  # Encontrar as linhas
  # Criar as linhas dos títulos
  linhas_titu <- grep(padrao_titu,variavel2)
  tam_lintitu <- length(linhas_titu)
  seq_titu <- seq(1,tam_lintitu,2)
  linhas_titulo <- linhas_titu[seq_titu]

  # Criar as demais linhas
  linhas_aut <- grep(padrao_aut,variavel2)
  linhas_ano <- grep(padrao_ano,variavel2)
  linhas_jorn <- grep(padrao_jorn,variavel2)
  linhas_vol <- grep(padrao_vol,variavel2)
  linhas_num <- grep(padrao_num,variavel2)
  linhas_pag <- grep(padrao_pag,variavel2)
  linhas_doi <- grep(padrao_doi,variavel2)
  linhas_abs <- grep(padrao_abs,variavel2)
  linhas_afili <- grep(padrao_afili,variavel2)
  linhas_issn <- grep(padrao_issn,variavel2)
  linhas_endemail <- grep(padrao_endemail,variavel2)
  linhas_nota <- grep(padrao_nota,variavel2)
  linhas_pachave <- grep(padrao_pachave,variavel2)

  # Criar os tamanhos das variáveis
  tamanho_aut <- length(linhas_aut)
  tamanho_titulo <- length(linhas_titulo)
  tamanho_ano <- length(linhas_ano)
  tamanho_jorn <- length(linhas_jorn)
  tamanho_vol <- length(linhas_vol)
  tamanho_num <- length(linhas_num)
  tamanho_pag <- length(linhas_pag)
  tamanho_doi <- length(linhas_doi)
  tamanho_abs <- length(linhas_abs)
  tamanho_afili <- length(linhas_afili)
  tamanho_issn <- length(linhas_issn)
  tamanho_endemail <- length(linhas_endemail)
  tamanho_nota <- length(linhas_nota)
  tamanho_pachave <- length(linhas_pachave)

  # Criar as variáveis
  autor <- variavel2[linhas_aut]
  ano <- variavel2[linhas_ano]
  titulo <- variavel2[linhas_titulo]
  jornal <- variavel2[linhas_jorn]
  volume <- variavel2[linhas_vol]
  numero <- variavel2[linhas_num ]
  pagina <- variavel2[linhas_pag]
  doi <- variavel2[linhas_doi]
  resumo <- variavel2[linhas_abs]
  afiliacao <- variavel2[linhas_afili]
  issn <- variavel2[linhas_issn]
  endemail <- variavel2[linhas_endemail]
  nota <- variavel2[linhas_nota]
  pachave <- variavel2[linhas_pachave]

  # Criar a lista de linhas das variáveis
  linhas_variaveis <- list(linhas_aut, linhas_titulo, linhas_ano,
                           linhas_jorn, linhas_vol, linhas_num,
                           linhas_pag, linhas_doi, linhas_abs,
                           linhas_afili, linhas_issn, linhas_endemail,
                           linhas_nota,linhas_pachave)

  # Criar o conjunto com os tamanhos das variáveis
  tamanhos_variaveis <- c(tamanho_aut, tamanho_titulo, tamanho_ano,
                          tamanho_jorn, tamanho_vol, tamanho_num,
                          tamanho_pag, tamanho_doi, tamanho_abs,
                          tamanho_afili, tamanho_issn, tamanho_endemail,
                          tamanho_nota,tamanho_pachave)

  # Criar conjunto das variáveis de interesse
  variaveis <- list(autor, titulo, ano, jornal, volume, numero,
                    pagina, doi, resumo, afiliacao, issn, endemail,
                    nota,pachave)

  # Criar a variavel do teste
  teste_presencaut <- numeric(num_linhasarticle)

  # Gerar as faixas de listas
  faixas <- list()

  # Criar o laço
  for(i in 1:(num_linhasarticle-1)){

    inicio <- linhas_article[i]
    fim <- (linhas_article[i+1]-1)
    faixa <- seq(inicio,fim,1)
    faixas[[i]] <- faixa

  }

  # Criação da última faixa
  inicio <- linhas_article[(num_linhasarticle)]
  fim <- linhas_chavefim
  faixa <- seq(inicio,fim,1)
  faixas[[num_linhasarticle]] <- faixa

  # Criar a função das colunas
  colunas_tabela <- function(tam_variavel,entrada_linhas,faixas,
                             num_linhasarticle,nome_variavel){
    # Criar as variáveis conjunto
    posicao_coluna <- numeric(tam_variavel)
    indices <- numeric(tam_variavel)
    linhas_coluna <- numeric(tam_variavel)
    numero_sco <- numeric(tam_variavel)

    # Inicializar os contadores
    j <- 0
    cont <- 0

    # Criar o laço
    for(i in 1:tam_variavel){

      #linhas_colun <- entrada_linhas[i]
      #j <- 0

      linhas_colun <- entrada_linhas[i]
      cont <- j

      repeat {

        cont <- cont + 1
        teste_present <- linhas_colun %in% faixas[[cont]]

        if(teste_present == TRUE)
          break

        j <- cont

      }

      posicao_coluna[i] <- cont

    }

    # Criar a variável da coluna
    posicao_geral <- character(num_linhasarticle)
    nome_geral <- character(num_linhasarticle)
    posicao_geral[posicao_coluna] <- posicao_coluna
    nome_geral[posicao_coluna] <- nome_variavel

    return(nome_geral)

  }

  # Criação das variáveis da coluna de saída
  tamanhos_colunas <- length(variaveis)
  colunas_variaveis <- list()

  # Criar o laço
  for (i in 1:tamanhos_colunas){

    # Chama as variáveis
    tam_variavel <- tamanhos_variaveis[i]
    linha_variavel <- linhas_variaveis[[i]]
    nome_variavel <- variaveis[[i]]
    entrada_linhas <- linha_variavel

    # Chama a função das colunas
    posicoes <- colunas_tabela(tam_variavel,entrada_linhas,faixas,
                               num_linhasarticle,nome_variavel)
    colunas_variaveis[[i]] <- posicoes

  }

  # Cria a tabela com as colunas
  tabela_colu <- cbind(colunas_variaveis[[1]],colunas_variaveis[[2]],
                       colunas_variaveis[[3]],colunas_variaveis[[4]],
                       colunas_variaveis[[5]],colunas_variaveis[[6]],
                       colunas_variaveis[[7]],colunas_variaveis[[8]],
                       colunas_variaveis[[9]], colunas_variaveis[[10]],
                       colunas_variaveis[[11]],colunas_variaveis[[12]],
                       colunas_variaveis[[13]],colunas_variaveis[[14]])

  # Muda o nome das colunas
  tabela_colu <- as.data.frame(tabela_colu)
  nome <- c("autor","titulo","ano","jornal","volume","numero",
            "pagina","doi","resumo","afiliacao","issn","endereco",
            "nota","pachave")
  colnames(tabela_colu) <- nome

  # Criar as veriáveis
  autor <- tabela_colu$autor
  titulo <- tabela_colu$titulo
  ano <- tabela_colu$ano
  jornal <- tabela_colu$jornal
  volume <- tabela_colu$volume
  numero <- tabela_colu$numero
  pagina <- tabela_colu$pagina
  doi <- tabela_colu$doi
  resumo <- tabela_colu$resumo
  afiliacao <- tabela_colu$afiliacao
  issn <- tabela_colu$issn
  endereco <- tabela_colu$endereco
  nota <- tabela_colu$nota
  pachave <- tabela_colu$pachave

  # Retirar as chaves
  autor <- sub("author = \\{","",autor)
  autor <- sub("\\},","",autor)
  titulo <- sub("title = \\{","",titulo)
  titulo <- sub("\\},","",titulo)
  ano <- sub("year = \\{","",ano)
  ano <- sub("\\},","",ano)
  jornal <- sub("journal = \\{","",jornal)
  jornal <- sub("\\},","",jornal)
  volume <- sub("volume = \\{","",volume)
  volume <- sub("\\},","",volume)
  numero <- sub("number = \\{","",numero)
  numero <- sub("\\},","",numero)
  pagina <- sub("pages = \\{","",pagina)
  pagina <- sub("\\},","",pagina)
  doi <- sub("doi = \\{","",doi)
  doi <- sub("\\},","",doi)
  resumo <- sub("abstract = \\{","",resumo)
  resumo <- sub("\\},","",resumo)
  afiliacao <- sub("affiliations = \\{","",afiliacao)
  afiliacao <- sub("\\},","",afiliacao)
  issn <- sub("issn = \\{","",issn)
  issn <- sub("\\},","",issn)
  endereco <- sub("correspondence_address = \\{","",endereco)
  endereco <- sub("\\},","",endereco)
  nota <- sub("note = \\{","",nota)
  nota <- sub("\\},","",nota)
  pachave <- sub("author_keywords = \\{","",pachave)
  pachave <- sub("\\},","",pachave)

  nomes_variaveis <- c("autor","titulo","ano","jornal","volume",
                       "numero","pagina","doi","resumo",
                       "afiliacao","issn","endereco","nota","pachave")

  colun_variavel <- list()
  tabela_colun <- list(autor,titulo,ano,jornal,volume,numero, pagina,
                       doi,resumo, afiliacao,issn,endereco,nota,pachave)
  tam_colun <- length(tabela_colun)

  for (i in 1:tam_colun){

    variavel <- tabela_colun[[i]]

    for (k in 1:num_linhasarticle){


      if(variavel[k] == "0"){

        variavel[k] <- ""

      }else{

        variavel[k] <- variavel[k]
      }


    }

    colun_variavel[[i]] <- variavel
  }

  # Cria a tabela com as colunas
  tabela_colunas <- cbind(colun_variavel[[1]], colun_variavel[[2]],
                          colun_variavel[[3]], colun_variavel[[4]],
                          colun_variavel[[5]], colun_variavel[[6]],
                          colun_variavel[[7]], colun_variavel[[8]],
                          colun_variavel[[9]], colun_variavel[[10]],
                          colun_variavel[[11]], colun_variavel[[12]],
                          colun_variavel[[13]], colun_variavel[[14]])
  tabela_colunas <- as.data.frame(tabela_colunas)
  nomes_variaveis <- c("autor","titulo","ano","jornal","volume",
                       "numero","pagina","doi","resumo","afiliacao",
                       "issn","endereco","nota","pachave")
  colnames(tabela_colunas) <- nomes_variaveis
  #####

  # Chamar a função
  nota_separ <- function(entrada_nota){

    # Teste inicial
    padrao_open <- "Open"
    linhas_open <- grep(padrao_open,tabela_colunas$nota)
    acesso_abe <- tabela_colunas$nota[linhas_open]
    acesso_abe <- strsplit(acesso_abe, split = "; ")

    # Criar variáveis
    tam_open <- length(linhas_open)
    tamanhos <- numeric(tam_open)
    acesso_aberto <- list()

    for (i in 1:tam_open){

      padrao_openint <- "Open"
      linhas_openint <- grep(padrao_openint,acesso_abe[[i]])
      min_linh <- min(linhas_openint)
      max_linh <- max(linhas_openint)
      sequen_int <- min_linh: max_linh
      acess_abert <- unlist(acesso_abe[[i]])
      acesso_aberto[[i]] <- acess_abert[sequen_int]
      tam_openaber <- length(acesso_aberto)
      tamanhos[i] <- tam_openaber
    }

    tama_open <- numeric(tam_open)
    open_abert <- character(tam_open)

    op_aberto <- character(tam_open)
    tam_openabe <- character(tam_open)

    # Construir o laço
    for (i in 1:tam_open){

      tam_openaber <- length(acesso_aberto[[i]])
      abert <- character(tam_openaber)
      tam_openabe[i] <- tam_openaber

      if(tam_openaber == 1){

        abert <- character(tam_openaber)
        abert[tam_openaber] <- acesso_aberto[[tam_openaber]][tam_openaber]

      } else if (tam_openaber == 2){

        sepa <- "; "
        abert[tam_openaber] <- paste0(acesso_aberto[[i]][tam_openaber-1],
                                      sepa,
                                      acesso_aberto[[i]][tam_openaber])

      }else{

        abert <- character(tam_openaber)
        abert[1] <- acesso_aberto[[i]][1]
        sepa <- "; "

        for (j in 2:(tam_openaber-1)){

          abert[j] <- paste0(abert[j-1],sepa,
                             acesso_aberto[[i]][j])
        }
        abert[tam_openaber] <- paste0(abert[tam_openaber-1],sepa,
                                      acesso_aberto[[i]][tam_openaber])
      }

      op_aberto[i] <- abert[tam_openaber]

    }

    # Exclui a chave final e ajusta
    op_aberto <- sub("}","",op_aberto)
    aberto <- character(num_linhasarticle)
    aberto[linhas_open] <- op_aberto

    # Criar o total de citações do artigo
    citacao <- numeric(num_linhasarticle)

    for (i in 1:num_linhasarticle){

      variavel <- tabela_colunas$nota[i]
      cita <- strsplit(variavel, split = "; ")
      citac <- cita[[1]][1]
      citaca <- strsplit(citac[[1]][1], split = "Cited by: ")
      citaca <- unlist(citaca)
      citacao[i] <- citaca[2]

    }
    citacao <- sub("}","",citacao)
    eissn <- character(num_linhasarticle)
    eissn <- ""

    # Criar tabela de saida
    tabela_nota <- cbind(eissn, aberto, citacao)
    tabela_nota <- as.data.frame(tabela_nota)

    # Saida da função
    return(tabela_nota)

  }
  ######
  # Chamar a função
  entrada_nota <- tabela_colunas$nota
  tabela_nota <- nota_separ(entrada_nota)

  # Criar as variáveis
  eissn <- tabela_nota$eissn
  aberto <- tabela_nota$aberto
  citacao <- tabela_nota$citacao

  tabela_colunafim <- cbind(tabela_colunas$autor,tabela_colunas$titulo,
                            tabela_colunas$ano,tabela_colunas$jornal,
                            tabela_colunas$volume,tabela_colunas$numero,
                            tabela_colunas$pagina,tabela_colunas$doi,
                            tabela_colunas$resumo,tabela_colunas$afiliacao,
                            tabela_colunas$issn,eissn,
                            tabela_colunas$endereco,aberto,
                            citacao,tabela_colunas$pachave)
  tabela_colunas <- tabela_colunafim
  tabela_colunas <- as.data.frame(tabela_colunas)
  nomes_variaveis <- c("autor","titulo","ano","jornal","volume",
                       "numero","pagina","doi","resumo","afiliacao",
                       "issn","eissn","endereco","aberto",
                       "citacao","palavra_chave")
  colnames(tabela_colunas) <- nomes_variaveis


  # Função para ajeitar os nomes dos autores
  equipe_nomesmodi <- function(variavel,num_linhasarticle){

    sequencia <- 1:num_linhasarticle
    padrao_and <- " and "
    linhas_multiplo <- grep(padrao_and,variavel)
    linhas_unico <- sequencia[-linhas_multiplo]

    # Chamar a função de conversão para multiplos autores
    convert_multiplo <- function(nomes, sobre_nomes, num_au){

      tam_equipe <- num_au

      letras_autores <- function(nomes, tam_equipe){

        # Entrada de parâmetros
        entrada_nome <- nomes

        letras_nomejun <- function(entrada_nome, tam_equipe){

          nome_auto <- strsplit(entrada_nome, split = " ")
          nome_auto <- unlist(nome_auto)
          tam_nomeauto <- length(nome_auto)

          # Separar as letras iniciais
          letras <- substr(nome_auto,1,1)

          # Criar o conjunto de letras
          letras_juntas <- character(tam_nomeauto)
          letras_juntas[1] <- letras[1]

          # Criar o laço
          for (j in 2:(tam_nomeauto-1)){

            letras_juntas[j] <- paste0(letras_juntas[j-1],
                                       letras[j])

          }
          # Criar a última letra
          letras_juntas[tam_nomeauto] <- paste0(letras_juntas[tam_nomeauto-1],
                                                letras[tam_nomeauto])
          letra_nome <- letras_juntas[tam_nomeauto]

          return(letra_nome)

        }

        # Aplicar a função para a equipe de autores
        letras_autor <- c()

        for (i in 1:tam_equipe){

          entrada_nome <- nomes[i]
          letra_nome <- letras_nomejun(entrada_nome,tam_equipe)
          letras_autor[i] <- letra_nome

        }

        return(letras_autor)

      }

      letras_nomemulti <- letras_autores(nomes, tam_equipe)
      nomefinal_multiplo  <- paste(sobre_nomes, sep =" ",
                                   letras_nomemulti)

      # Criar equipe de autores
      equipe_autores <- character(tam_equipe)
      # Criar o primeiro autor
      equipe_autores[1] <- nomefinal_multiplo[1]

      # Criar o laço
      for (i in 2:(tam_equipe-1)){

        equipe_autores[i] <- paste(equipe_autores[i-1], sep = ";",
                                   nomefinal_multiplo[i])

      }

      # Acrescentar o último autor
      equipe_autores[tam_equipe] <- paste(equipe_autores[tam_equipe-1],
                                          sep = ";", nomefinal_multiplo[tam_equipe])
      equipe_autores <- equipe_autores[tam_equipe]

      return(equipe_autores)

    }

    # Criar as equipes de nomes
    baseautor_multiplo <- tabela_colunas[linhas_multiplo,]
    variavel <- baseautor_multiplo$autor
    m <- length(variavel)
    equipes_multip <- list()

    for (k in 1:m){

      #autors <- basededados$autor[k]
      autors <- variavel[k]
      entrada_autors <- autors

      # Separar os autores
      autores <- entrada_autors
      autores <- strsplit(autors, split = " and ")
      autores <- unlist(autores)
      autores <- toupper(autores)
      num_au <- length(autores)

      num_aut <- 2*num_au
      autores <- strsplit(autores, split = ", ")
      autores <- unlist(autores)

      # Criar as sequencias par e impar
      seq_autimp <- seq(1,num_aut,2)
      seq_autpar <- seq_autimp + 1

      # Criar os sobrenomes e nomes
      sobre_nomes <- autores[seq_autimp]
      nomes <- autores[seq_autpar]

      # Chama a função da conversão dos nomes
      equipe_multi <- convert_multiplo(nomes, sobre_nomes, num_au)
      equipes_multip[[k]] <- equipe_multi
    }

    # Excluir o ponto vírgula antes de alguns nomes
    teste_acent <- substr(equipes_multip,1,1)
    padrao_acent <- ";"
    linha_acent <- grep(padrao_acent,teste_acent)
    equipes_multip[linha_acent] <- sub(";","",equipes_multip[linha_acent])

    # Criar a lista de nomes final
    lista_resultadogeral <- list()
    resultado <- equipes_multip[[1]]
    lista_resultadogeral[[1]] <- resultado

    # Laço para gerar a tabela de resultados
    for(i in 2:m){

      resultado <- equipes_multip[[i]]
      lista_resultadogeral[[i]] <- rbind(lista_resultadogeral[[i-1]], resultado)

    }

    # Gerar lista final e caminho para exportação
    resultado_final <- lista_resultadogeral[[m]]
    resultado_final <- as.data.frame(resultado_final)
    colnames(resultado_final) <- "autor"

    # Mostrar a saída final de autores
    equipe_multiplo <- resultado_final$autor
    # Fazer alteração nos nomes
    tabela_colunas$autor[linhas_unico] <- toupper(tabela_colunas$autor[linhas_unico])
    tabela_colunas$autor[linhas_multiplo] <- equipe_multiplo

    return(tabela_colunas$autor)

  }

  # Criar saida final
  variavel <- tabela_colunas$autor
  autores <- equipe_nomesmodi(variavel,num_linhasarticle)
  tabela_colunas$autor <- autores
  tabela_scopus <- cbind(tabela_colunas)

  return(tabela_scopus)

}
