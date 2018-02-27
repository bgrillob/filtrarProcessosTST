# https://stackoverflow.com/questions/46224378/how-to-submit-a-form-that-seems-to-be-handled-by-javascript-using-httr-or-rvest
# https://stackoverflow.com/questions/39516673/rvest-could-not-find-possible-submission-target-when-submitting-form
# https://www.datacamp.com/community/tutorials/scraping-javascript-generated-data-with-r
# https://htmledit.squarefree.com/
# https://stackoverflow.com/questions/46304664/how-to-deal-with-captcha-when-web-scraping-using-r
# https://stackoverflow.com/questions/41466263/r-change-ip-address-programatically
library(httr)
library(magrittr)
library(tidyverse)
library(rvest)
require(xml2)
rm(list = ls())
setwd("/home/bgrillob/processosTST")
# COLETAR PROCESSOS EM PREVISÃO DE PAUTA ----
processosJulgar <- html_session(
  "http://aplicacao5.tst.jus.br/consultapauta/pautaForm.do?relatorProcesso=GMMHM&codOrgaoJudic=74"
  ) %>%
  html_nodes("table") %>%
  .[[3]] %>%
  html_table()
# FORMATAR DADOS ----
processosJulgar <- strsplit(processosJulgar$X1, split = " - ") %>%
  do.call(rbind, .) %>%
  set_colnames(c("tipoProcesso", "Processo")) %>%
  data.frame(
    Turma = "T2", ., stringsAsFactors = FALSE
  )

# DADOS PREENCHIMENTO FORMULÁRIO ----
#processosJulgar <- read.table("baseProcessos.csv", sep = ";", header = TRUE, stringsAsFactors = FALSE)
# FUNÇÃO GERAR CAMPOS FORMULÁRIO
separarItens <- function(x) {
  resultado <- x %>%
    strsplit(., split = "-") %>%
    unlist %>%
    strsplit(., split = "\\.") %>%
    unlist %>%
    matrix(., nrow = 1) %>%
    as.data.frame(., stringsAsFactors = FALSE) %>%
    set_colnames(c("Número",	"Dígito",	"Ano",	"Órgao",	"Tribunal",	"Vara"))
  resultado$Número <- sprintf("%07d", as.numeric(resultado$Número))
  return(resultado)
}

processosJulgar <- data.frame(
  processosJulgar, 
  processosJulgar$Processo %>%
    map(separarItens) %>%
    do.call(rbind, .), 
  stringsAsFactors = FALSE)

# SITE ----
  # A URL ONDE HÁ FORMULÁRIOS É UMA CONTENT INNER QUE CHAMA A APLICAÇÃO NO PRÓXIMO SITE
url <- "http://www.tst.jus.br/processos-do-tst" 
  # URL DA APLICAÇÃO
url <- "http://aplicacao4.tst.jus.br/consultaProcessual/"

# EXTRAIR DADOS ----
  # FUNÇÃO EXTRAIR TABELA EVOLUÇÃO PROCESSO ----
w <- 1
valoresRef <- processosJulgar[w,]

formularioParaTabela <- function(valoresRef, site, 
                                 IP = NULL, PORT = NULL) {
      # ESTABELECER CONEXAO COM O SITE
  if (!(is.null(IP) && is.null(PORT))) {
    siteProcessos <- html_session(site, use_proxy(url = IP, port = PORT))
  } else {
    siteProcessos <- html_session(site) 
  }

  formularioInc <- siteProcessos %>%
    html_nodes("form") %>%
    html_form() 
  caminho <- formularioInc[[1]]
      # CAMPOS PREENCHER NO FORMULÁRIO
  formularioComp <- caminho %>%
    set_values(
      numeroTst = as.numeric(valoresRef$Número),
      digitoTst = as.numeric(valoresRef$Dígito),
      anoTst = as.numeric(valoresRef$Ano),
      orgaoTst = as.numeric(valoresRef$Órgao),
      tribunalTst = as.numeric(valoresRef$Tribunal),
      varaTst = as.numeric(valoresRef$Vara)
    )
      # ENVIAR FORMULÁRIO
  sessao <- submit_form(siteProcessos, formularioComp)
      # PEGAR NÓS QUE SÃO TABELA E TRANSFORMAR EM DATA FRAME
  tabelas <- sessao %>% 
    html_nodes("table") 
  textoParaTabela <- tabelas[[11]] %>%
    html_text() %>%
    gsub(pattern = "(\r|<br />)", replacement = "") %>%
    gsub(pattern = "(\n|<br />)", replacement = "") %>%
    gsub(pattern = "(\t|<br />)", replacement = ";") %>%
    strsplit(split = ";") %>%
    unlist() %>%
    {.[!(. %in% c("", "Histórico do processo"))]}
  nTxt <- length(textoParaTabela)
  tabelaRes <- data.frame(
    Data = textoParaTabela[seq(1, nTxt, 2)],
    Evolucao = textoParaTabela[seq(0, nTxt, 2)],
    stringsAsFactors = FALSE
  )
return(tabelaRes)
#return(headers(siteProcessos))
}


# ESTRUTURAR TABELA PARA COLETA ----
processosJulgar <- split(processosJulgar, seq(nrow(processosJulgar)))
tabelasProcessos <- vector("list", length(processosJulgar))
for (w in seq_along(processosJulgar)) {
  tabelasProcessos[[w]] <- formularioParaTabela(valoresRef = processosJulgar[[w]], site = url)
  tempoEsperar <- runif(1, min = 1, max = 5)
  print(tempoEsperar)
  Sys.sleep(tempoEsperar) # INSERIR TEMPO DE ESPERA PRA EVITAR CAPTCHA
}

