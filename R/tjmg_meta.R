#' Function tjmg_meta
#'
#' This function retrieves the metadata related to the high court decisions according to a search.
#'
#' @param lawsuit  vector of lawsuit numbers according to CNJ rules. It can be either with punctuation
#'  or just numbers
#' @param complete_data logical. Default is TRUE. Returns all available information about the lawsuit
#' @param course logical. Default is FALSE. Returns the course of the lawsuit until the last update
#' @keywords Courts, Decisions, Jurimetry, Webscraping
#' @export
#' @examples
#' tjmg(....)


mg_url<-function(lawsuit,complete_data=TRUE,course=FALSE)

lawsuit<-str_replace_all(lawsuit,"\\D","")

stopifnot(str_length(lawsuit)==20, error=stop("The number of digits must be exactely 20")) 

stopifnot(!str_detect(lawsuit,"\\D"),error=stop("all characters must be digits"))

mg_url<-str_c("http://www4.tjmg.jus.br/juridico/sf/proc_resultado.jsp?tipoPesquisa=1&txtProcesso=",lawsuit,"&comrCodigo=",str_extract(lawsuit,"\\d{4}$"),"&nomePessoa=&tipoPessoa=X&naturezaProcesso=0&situacaoParte=X&codigoOAB=&tipoOAB=N&ufOAB=MG&numero=1&select=1&tipoConsulta=1&natureza=0&ativoBaixado=X&listaProcessos=",lawsuit)


course<- mg_url %>%
  map_dfr(possibly(~{
    .x %>%
      read_html() %>% 
      html_nodes(xpath="//td[2]/b/a") %>% 
      html_attr("href") %>% 
      data.frame(x1=.,x2=.x)
  },data.frame(x1=NA,x2=.)))

course<-unique(course)



