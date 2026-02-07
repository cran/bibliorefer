#' Function that converts the BibTeX format from the WoS database to a CSV spreadsheet.
#'
#' The function converts the WoS database metadata file from Bibtex format to a CSV spreadsheet. The function's input is a file with the .bib extension. The function returns a dataframe.
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
#' #Call the convert_bibliowos function
#'
#' file_db <- system.file("extdata","savedrecs.bib", package = "bibliorefer")
#' cabec_wos = TRUE
#' quoaspa_wos = ""
#' arquivos <- entrada_wos(file_db, cabec_wos, quoaspa_wos)
#'
#' #Call the convert_bibliowos function
#' font_wos <- convert_bibliowos(arquivos)
#' font_wos
#'
#'
#'
convert_bibliowos <- function(arquivos){

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

    # Criar a base de dados da WiS
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

  # Mostrar linha incompleta e autor incompleto
  linha_difere <- function(lin_incmpi){

    auto_difincmp <- variavel1[lin_incmpi]
    lin_incmp <- lin_incmpi

    repeat {

      lin_incmp <- lin_incmp + 1
      auto_difincmp <- variavel1[lin_incmp]
      padrao_chf <- "},"
      teste_finch <- grepl(padrao_chf,auto_difincmp)

      if(teste_finch == "TRUE")
        break

    }

    lin_incmpf <- lin_incmp
    # Criar sequẽncia de linhas e autores
    seq_lincmp <- lin_incmpi:lin_incmpf
    seq_lin <- seq_lincmp
    auto_difincmp <- variavel1[seq_lin]
    saida <- list(seq_lin,auto_difincmp)
    return(saida)

  }

  # Excluir livros autores
  padrao_BA <- "Book-Author = "
  linha_BA <- grep(padrao_BA,variavel1)
  tama_linhaBA <- length(linha_BA)

  # Excluir livros autores
  padrao_BGA <- "Book-Group-Author ="
  linha_BGA <- grep(padrao_BGA,variavel1)
  tama_linhaBGA <- length(linha_BGA)
  linha_exc <- c(linha_BA,linha_BGA)
  tam_linexc <- length(linha_exc)

  if (tam_linexc == 0){

    variavel1 <- variavel1

  }else{

    # Mostra os autores
    auto_dif <- variavel1[linha_exc]

    # Fazer o teste
    padrao_chf <- "},"
    situ_compincomp <- grepl(padrao_chf,auto_dif)
    padrao_cmp <- "TRUE"
    padrao_incmp <- "FALSE"
    lin_cm <- grep(padrao_cmp,situ_compincomp)
    lin_incm <- grep(padrao_incmp,situ_compincomp)
    lin_cmp <- linha_exc[lin_cm]
    auto_difcomp <- variavel1[lin_cmp]

    lin_incmpi <- linha_exc[lin_incm]
    tam_lincm <- length(lin_incm)
    saida_linfunc <- list()
    saida_nomefunc <- list()

    for(j in 1:tam_lincm){

      linha_incmpi <- lin_incmpi[j]
      saida <- linha_difere(linha_incmpi)
      saida_linfunc[[j]] <- saida[[1]]
      saida_nomefunc[[j]] <- saida[[2]]

    }

    saida_nomefunc <- unlist(saida_nomefunc)
    saida_linfunc <- unlist(saida_linfunc)
    auto_difincomp <- variavel1[saida_linfunc]
    auto_dife <- c(auto_difcomp,auto_difincomp)
    lin_exc <- c(saida_linfunc,lin_cmp)
    variavel1 <- variavel1[-lin_exc]

  }

  # Criar a linha das entradas
  padrao_article <- "\\{ WOS"
  linhas_article <- grep(padrao_article,variavel1)
  num_linhasarticle <- length(linhas_article)

  # Criar as linhas das chaves finais
  linha_cha <- linhas_article - 1
  linha_cha <- linha_cha[-1]
  linha_chafim <- length(variavel1)
  linhas_chave <- c(linha_cha,linha_chafim)

  # Criar a largura das faixas
  sequencia1 <- 1:(num_linhasarticle-1)
  sequencia2 <- 2:num_linhasarticle
  larg_faixa <- linhas_article[sequencia2]-linhas_article[sequencia1]
  larg_faixasfi <- linhas_chave[num_linhasarticle] - linhas_article[num_linhasarticle]
  larg_faixas <- c(larg_faixa,larg_faixasfi)

  # Encontrar as linhas dos autores
  padrao_aut <- "Author ="
  padrao_titu <- "Title ="
  padrao_ano <- "Year = "
  padrao_jorn <- "Journal ="
  padrao_vol <- "Volume ="
  padrao_num <- "Number ="
  padrao_pag <- "Pages = "
  padrao_doi <- "DOI ="
  padrao_abs <- "Abstract ="
  padrao_afili <- "Affiliation ="
  padrao_issn <- "ISSN ="
  padrao_eissn <- "EISSN ="
  padrao_endemail <- "Address = "
  padrao_aberto <- "OA = "
  padrao_citacao <- "Times-Cited = "
  padrao_pachav <- "Keywords = "

  # Criar as demais linhas
  linhas_autores <- grep(padrao_aut,variavel1)
  linhas_titu <- grep(padrao_titu,variavel1)
  linhas_ano <- grep(padrao_ano,variavel1)
  linhas_jorn <- grep(padrao_jorn,variavel1)
  linhas_vol <- grep(padrao_vol,variavel1)
  linhas_num <- grep(padrao_num,variavel1)
  linhas_pag <- grep(padrao_pag,variavel1)
  linhas_doi <- grep(padrao_doi,variavel1)
  linhas_abs <- grep(padrao_abs,variavel1)
  linhas_afili <- grep(padrao_afili,variavel1)
  linhas_issn <- grep(padrao_issn,variavel1)
  linhas_eissn <- grep(padrao_eissn,variavel1)
  linhas_endemail <- grep(padrao_endemail,variavel1)
  linhas_aberto <- grep(padrao_aberto,variavel1)
  linhas_citacao <- grep(padrao_citacao,variavel1)
  linhas_pachav <- grep(padrao_pachav,variavel1)

  # Criar os tamanhos das variáveis
  tamanho_aut <- length(linhas_autores)
  tamanho_titu <- length(linhas_titu)
  tamanho_ano <- length(linhas_ano)
  tamanho_jorn <- length(linhas_jorn)
  tamanho_vol <- length(linhas_vol)
  tamanho_num <- length(linhas_num)
  tamanho_pag <- length(linhas_pag)
  tamanho_doi <- length(linhas_doi)
  tamanho_abs <- length(linhas_abs)
  tamanho_afili <- length(linhas_afili)
  tamanho_issn <- length(linhas_issn)
  tamanho_eissn <- length(linhas_eissn)
  tamanho_endemail <- length(linhas_endemail)
  tamanho_aberto <- length(linhas_aberto)
  tamanho_citacao <- length(linhas_citacao)
  tamanho_pachav <- length(linhas_pachav)

  tamanhos_variaveis <- c(tamanho_aut, tamanho_titu, tamanho_ano,
                          tamanho_jorn, tamanho_vol, tamanho_num,
                          tamanho_pag, tamanho_doi, tamanho_abs,
                          tamanho_afili, tamanho_issn, tamanho_eissn,
                          tamanho_endemail, tamanho_aberto,
                          tamanho_citacao, tamanho_pachav)

  # Organizar os autores
  organ_variav <- function(variavel1,linhas_variavel){

    # Entrada de dados
    linhas_auto <- linhas_variavel

    # Charmar a função
    reune_vari <- function(entr_aut, entr_linhas){

      # Criar as variáveis
      autores <- entr_aut
      linhas_autores <- entr_linhas

      # Autor com nome completo
      padrao_autcomp <- "},"

      # Posição dos autores completos
      posicao_comp <- grep(padrao_autcomp, autores)
      lin_autcomp <- linhas_autores[posicao_comp]
      teste_autincomp <- linhas_autores %in% lin_autcomp

      # Autor com nome incompleto
      posicao <- grep("FALSE",teste_autincomp)
      lin_autincomp1 <- linhas_autores[posicao]
      lin_autincomp2 <- lin_autincomp1 + 1

      # Saída da função
      linhas_retorno <- list(posicao,lin_autincomp1,lin_autincomp2,
                             posicao_comp,lin_autcomp)

      return(linhas_retorno)

    }

    # Chamar a função de reunião de variáveis
    entr_aut <- variavel1[linhas_auto]
    entr_linhas <- linhas_auto
    linha <- reune_vari(entr_aut, entr_linhas)

    # Chamar a função de organização simples das variáveis
    org_vari <- function(variavel1,linha){

      posicao <- linha[[1]]
      lin_autincomp1 <- linha[[2]]
      lin_autincomp2 <- linha[[3]]
      posicao_completa <- linha[[4]]
      linha_completa <- linha[[5]]
      aut_completo <- variavel1[linha_completa]

      tama_posicao <- length(posicao)

      # Criar as parte dos nomes e colar
      au_incomp1 <- variavel1[lin_autincomp1]
      au_incomp2 <- variavel1[lin_autincomp2]
      au_incomp2 <- sub("  ","",au_incomp2)
      aut_incomp <- paste0(au_incomp1,au_incomp2)
      aut_incomp1 <- aut_incomp

      # Criar variáveis e gerar o laço
      autors <- list()
      autors[[1]] <- aut_incomp
      posicoes <- list()
      posicoes[[1]] <- posicao

      j <- 1
      repeat {

        j <- j + 1

        #Entrada de dados
        entr_aut <- aut_incomp
        entr_linhas <- lin_autincomp2

        # Chama a função
        linha <- reune_vari(entr_aut, entr_linhas)

        posicao <- linha[[1]]
        lin_autincomp1 <- linha[[2]]
        lin_autincomp2 <- linha[[3]]

        au_incomp1 <- aut_incomp[posicao]
        au_incomp2 <- variavel1[lin_autincomp2]

        au_incomp2 <- sub("  ","",au_incomp2)
        aut_incomp <- paste0(au_incomp1,au_incomp2)

        autors[[j]] <- aut_incomp
        posicoes[[j]] <- posicao

        padrao_autcomp <- "},"
        autor_junto <- grepl(padrao_autcomp, autors[[j]])
        padrao_falso <- "FALSE"
        presen_falso <- grep(padrao_falso, autor_junto)
        quanti_falso <- length(presen_falso)

        if(quanti_falso == 0)
          break

      }

      incompleta <- list()
      teste_visao <- numeric()
      posico <- posicoes[[1]]
      posicao <- character(tama_posicao)
      posicoe <- character(tama_posicao)
      autor_comp <- character(tama_posicao)

      # Criar a quantidade final do loop
      taman_autors <- length(autors)

      # Gera as posições dos autores da primeira posição
      nome_autor <- autors[[1]]

      # Fazer o teste
      padrao_autcomp <- "},"
      situ_compincomp <- grepl(padrao_autcomp,nome_autor)
      padrao_comp <- "TRUE"
      padrao_incomp <- "FALSE"
      lin_comp <- grep(padrao_comp,situ_compincomp)
      posicao[lin_comp] <- lin_comp
      posicoe[lin_comp] <- posico[lin_comp]
      autor_comp[lin_comp] <- nome_autor[lin_comp]
      lin_incomp <- grep(padrao_incomp,situ_compincomp)
      incompleta <- list()
      teste_visao <- numeric(taman_autors)
      incompleta[[1]] <- lin_incomp

      # Encontra os autores para os casos restantes
      for (k in 2:taman_autors){

        nome_autor <- autors[[k]]
        posi_incompantes <- lin_incomp
        padrao_autcomp <- "},"
        situ_compincomp <- grepl(padrao_autcomp,nome_autor)
        padrao_comp <- "TRUE"
        padrao_incomp <- "FALSE"

        # Gera as posicoes atualizadas
        lin_com <- grep(padrao_comp,situ_compincomp)
        tam_lincom <- length(lin_com)
        teste_visao[k] <- tam_lincom

        if(tam_lincom != 0){

          # Mostra as posicões anteriores
          lin_comp <- posi_incompantes[lin_com]
          posicao[lin_comp] <- lin_comp
          posicoe[lin_comp] <- posico[lin_comp]

          # Posição dos autores completos
          autor_comp[lin_comp] <- nome_autor[lin_com]
          # Posição das linhas dos autores incompletos
          lin_incom <- grep(padrao_incomp,situ_compincomp)
          lin_incomp <- posi_incompantes[lin_incom]

        }else{

          autor_comp[lin_comp] <- autor_comp[lin_comp]
          lin_incomp <- lin_incomp

        }

        incompleta[[k]] <- lin_incomp

      }

      # Posicoes finais
      posi_global <- character(tamanho_aut)
      variavel_inte <- character(tamanho_aut)
      lin_global <- numeric(tamanho_aut)
      posicoe <- as.numeric(posicoe)
      posi_global[posicao_completa] <- posicao_completa
      posi_global[posicoe] <- posicoe

      # Gera a lista de autores
      variavel_inte[posicao_completa] <- aut_completo
      variavel_inte[posicoe] <- autor_comp

      return(variavel_inte)

    }

    # Chama a função de organização
    variavel_inte <- org_vari(variavel1,linha)
    posicao <- linha[[1]]
    saida <- list(variavel_inte,posicao)

    return(saida)

  }

  # Criar o laço
  vari_interes <- list()
  posicao_interes <- list()
  conju_linhas <- list(linhas_autores, linhas_titu,
                       linhas_abs, linhas_afili)
  tam_conju <- length(conju_linhas)

  for (i in 1:tam_conju){

    linhas_variavel <- conju_linhas[[i]]
    planilha <- organ_variav(variavel1,linhas_variavel)
    vari_interes[[i]] <- planilha[[1]]
    posicao_interes[[i]] <- planilha[[2]]

  }

  # Obter os nomes das variáveis
  autor <- vari_interes[[1]]
  titu <- vari_interes[[2]]
  resumo <- vari_interes[[3]]
  afili <- vari_interes[[4]]

  # Ajustar a quntidade de nomes das variáveis
  nome_modifi <- list(autor,titu,resumo, afili)
  tam_nomemodifi <- length(nome_modifi)
  nome_modific <- list()

  for(i in 1:tam_nomemodifi){

    nome_modifica <- nome_modifi[[i]]
    num_caracmodi <- nchar(nome_modifica)
    tama_nuncarac <- nchar(num_caracmodi)
    padrao_uni <- "1"
    situ_linhas <- grepl(padrao_uni,tama_nuncarac)
    padrao_fal <- "FALSE"
    padrao_verda <- "TRUE"
    linha_naovazi <- grep(padrao_fal,situ_linhas)
    nome_modificad <- nome_modifica[linha_naovazi]
    linha_uni <- grep(padrao_verda,situ_linhas)
    padrao_nulo <- "0"
    teste_nulo <- grepl(padrao_nulo,num_caracmodi[linha_uni])
    tama_uni <- length(linha_uni)

    if(tama_uni == 0){

      nome_modificad <- nome_modifica[linha_naovazi]

    }else{

      for(j in 1:tama_uni){

        teste_nul <- teste_nulo[j]

        if(teste_nul == TRUE){

          nome_modificad[linha_uni[j]] <- ""

        }else{

          nome_modificad[linha_uni[j]] <- nome_modifica[linha_uni[j]]

        }
      }

    }

    nome_modific[[i]] <- nome_modificad
  }

  # Ajustar as variǘaies número e issn
  numero <- variavel1[linhas_num]
  padrao_AN <- "Article-Number = "
  linha_AN <- grep(padrao_AN,numero)
  padrao_DDN <- "Doc-Delivery-Number = "
  linha_DDN <- grep(padrao_DDN,numero)
  linhas_excnum <- c(linha_AN,linha_DDN)
  linhas_num <- linhas_num[-linhas_excnum]

  issn <- variavel1[linhas_issn]
  padrao_eis <- "EISSN = "
  linha_eis <- grep(padrao_eis,issn)
  linhas_issn <- linhas_issn[-linha_eis]

  # Criar variáveis
  autor <- nome_modific[[1]]
  titu <- nome_modific[[2]]
  ano <- variavel1[linhas_ano]
  jornal <- variavel1[linhas_jorn]
  volume <- variavel1[linhas_vol]
  numero <- variavel1[linhas_num]
  pagina <- variavel1[linhas_pag]
  doi <- variavel1[linhas_doi]
  resumo <- nome_modific[[3]]
  afili <- nome_modific[[4]]
  issn <- variavel1[linhas_issn]
  eissn <- variavel1[linhas_eissn]
  endemail <- variavel1[linhas_endemail]
  aberto <- variavel1[linhas_aberto]
  citacao <- variavel1[linhas_citacao]
  pachav <- variavel1[linhas_pachav]

  # Conjunto de variáveis
  conju_vari <- list(autor, titu, ano, jornal, volume,
                     numero, pagina, doi, resumo, afili, issn, eissn,
                     endemail, aberto, citacao, pachav)

  # Criar a lista de linhas das variáveis
  linhas_variaveis <- list(linhas_autores, linhas_titu, linhas_ano,
                           linhas_jorn, linhas_vol, linhas_num,
                           linhas_pag, linhas_doi, linhas_abs,
                           linhas_afili, linhas_issn, linhas_eissn,
                           linhas_endemail, linhas_aberto,
                           linhas_citacao, linhas_pachav)

  # Tamanho das variáveis
  tam_varicon <- length(linhas_variaveis)
  tamanhos_variaveis <- numeric(tam_varicon)
  linhas_finais <- numeric(tam_varicon)

  for (i in 1:tam_varicon){

    tamanhos_variaveis[i] <- length(linhas_variaveis[[i]])
    linhas_finais[i] <- linhas_variaveis[[i]][tamanhos_variaveis[i]]

  }

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
  fim <- linha_chafim
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
    sequen_ini <- 1:tam_variavel

    posicao_geral <- character(num_linhasarticle)
    nome_geral <- character(num_linhasarticle)
    posicao_geral[posicao_coluna] <- posicao_coluna
    nome_geral[posicao_coluna] <- nome_variavel[sequen_ini]

    return(nome_geral)

  }

  # Criação das variáveis da coluna de saída
  tam_conjuvari <- length(conju_vari)
  colunas_variaveis <- list()

  # Criar o laço
  for (i in 1:tam_conjuvari){

    # Chama as variáveis
    tam_variavel <- tamanhos_variaveis[i]
    entrada_linhas <- linhas_variaveis[[i]]
    nome_variavel <- conju_vari[[i]]

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
                       colunas_variaveis[[13]],colunas_variaveis[[14]],
                       colunas_variaveis[[15]],colunas_variaveis[[16]])

  # Muda o nome das colunas
  tabela_colu <- as.data.frame(tabela_colu)
  nome <- c("autor","titulo","ano","jornal","volume","numero",
            "pagina","doi","resumo","afiliacao","issn","eissn",
            "endereco","aberto","citacao","palavra_chave")
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
  eissn <- tabela_colu$eissn
  endereco <- tabela_colu$endereco
  aberto <- tabela_colu$aberto
  citacao <- tabela_colu$citacao
  palavra_chave <- tabela_colu$`palavra_chave`

  # Retirar as chaves
  autor <- sub("Author = \\{","",autor)
  autor <- sub("\\},","",autor)
  titulo <- sub("Title = \\{","",titulo)
  titulo <- sub("\\},","",titulo)
  ano <- sub("Year = \\{","",ano)
  ano <- sub("\\},","",ano)
  jornal <- sub("Journal = \\{","",jornal)
  jornal <- sub("\\},","",jornal)
  volume <- sub("Volume = \\{","",volume)
  volume <- sub("\\},","",volume)
  numero <- sub("Number = \\{","",numero)
  numero <- sub("\\},","",numero)
  pagina <- sub("Pages = \\{","",pagina)
  pagina <- sub("\\},","",pagina)
  doi <- sub("DOI = \\{","",doi)
  doi <- sub("\\},","",doi)
  resumo <- sub("Abstract = \\{","",resumo)
  resumo <- sub("\\},","",resumo)
  afiliacao <- sub("Affiliation = \\{","",afiliacao)
  afiliacao <- sub("\\},","",afiliacao)
  issn <- sub("ISSN = \\{","",issn)
  issn <- sub("\\},","",issn)
  eissn <- sub("EISSN = \\{","",eissn)
  eissn <- sub("\\},","",eissn)
  endereco <- sub("Address = \\{","",endereco)
  endereco <- sub("\\},","",endereco)
  aberto <- sub("OA = \\{","",aberto)
  aberto <- sub("\\},","",aberto)
  citacao <- sub("Times-Cited = \\{","",citacao)
  citacao <- sub("\\},","",citacao)
  palavra_chave <- sub("Keywords = \\{","",palavra_chave)
  palavra_chave <- sub("\\},","",palavra_chave)

  # Criar a tabela final
  tabela_colunas <- cbind(autor,titulo,ano,jornal,volume,numero, pagina,
                          doi,resumo, afiliacao,issn,eissn,endereco,
                          aberto,citacao,palavra_chave)
  tabela_colunas <- as.data.frame(tabela_colunas)
  nome_variaveis <- c("autor","titulo","ano","jornal","volume","numero",
                      "pagina","doi","resumo","afiliacao","issn","eissn",
                      "endereco","aberto","citacao","palavra_chave")
  colnames(tabela_colunas) <- nome_variaveis

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
      #resultado <- as.data.frame(resultado)
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

  variavel <- tabela_colunas$autor
  autores <- equipe_nomesmodi(variavel,num_linhasarticle)
  tabela_colunas$autor <- autores

  return(tabela_colunas)

}
