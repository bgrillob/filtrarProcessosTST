# https://stackoverflow.com/questions/46224378/how-to-submit-a-form-that-seems-to-be-handled-by-javascript-using-httr-or-rvest
# https://stackoverflow.com/questions/39516673/rvest-could-not-find-possible-submission-target-when-submitting-form
# https://www.datacamp.com/community/tutorials/scraping-javascript-generated-data-with-r
# https://htmledit.squarefree.com/
library(httr)
library(magrittr)
library(tidyverse)
library(rvest)
require(xml2)
rm(list = ls())
setwd("/home/bgrillob/processosTST")
# FORMATAR DADOS ----
processosJulgar <- read.table("T2.csv", sep = ",", stringsAsFactors = FALSE)
processosJulgar <- strsplit(processosJulgar$V1, split = " - ") %>%
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
siteProcessos <- html_session(url)
formularioInc <- siteProcessos %>%
  html_nodes("form") %>%
  html_form() 
formularioInc <- formularioInc[[1]]

valoresRef <- processosJulgar[1,]
caminho <- formularioInc

formularioParaTabela <- function(valoresRef, caminho = formularioInc) {
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
  tabela <- tabelas[[11]] %>%
    html_table(fill = TRUE, header = FALSE) 
  resultado <- tabela %>%
    unlist() %>% 
    as.character()  %>%
    
}
