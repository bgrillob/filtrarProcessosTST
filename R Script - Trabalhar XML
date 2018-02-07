# https://rud.is/rpubs/xml2power/
# https://www.w3schools.com/xml/xpath_syntax.asp
library(xml2)
library(httr)
library(htmltools)
library(tidyverse)
rm(list = ls())
setwd("")
arquivoXML3 <- read_xml("mondial-3.0.xml")
arquivoXML2 <- read_xml("3525012017120001.xte")
arquivoXML <- read_xml("AOC_Location_Report.xml")


xtrct <- function(doc, target) { 
  xml_find_all(doc, target) %>%
    xml_text() %>% 
    trimws() 
}

xtrct_df <- function(doc, top) {
  xml_find_first(doc, sprintf(".//%s", top)) %>%
    xml_children() %>%
    xml_name() %>%
    map(~{
      xtrct(doc, sprintf(".//%s/%s", top, .x)) %>%
        list() %>%
        set_names(tolower(.x))
    }) %>%
    flatten_df() %>% # unlist()
    readr::type_convert()
}

doc <- arquivoXML3
xtrct_df(arquivoXML2, top = "ans:Procedimento")
xtrct_df(doc, top = 'province/city')
valoresDf <- xtrct_df(doc, top = "ans:valoresGuia")



# TESTE ARQUIVO MONDIAL ----
xml.file <- xmlParse("mondial-3.0.xml")
XML:::xmlAttrsToDataFrame(xml.file["//country"])
XML:::xmlAttrsToDataFrame(xml.file['//country/province'])
XML:::xmlAttrsToDataFrame(xml.file['//country[@name="Germany"]/province'])


# TESTE ARQUIVOS PLANT-CATALOG ----
xmlfile <- xmlTreeParse("plant_catalog.xml")
xmltop <- xmlRoot(xmlfile) # xmlRoot ACESSA O TOP NODE
plantcat <- xmlSApply(xmltop, function(x) 
  xmlSApply(x, xmlValue))
plantcat_df <- data.frame(t(plantcat), row.names = NULL)

