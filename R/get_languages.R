#' Get all languages in europarl
#'
#' @export
#' @import rvest
#' @import magrittr

get_languages <- function() {

  url <- "http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20161215+ITEM-007-03+DOC+XML+V0//EN&language=en&query=INTERV&detail=4-220-000"
  page <- read_html(url)
  language <- page %>%
    html_nodes(".language_select") %>%
    html_text()
  language <- gsub("\\(Selected\\)","",language) %>% strsplit("\n")
  return(unlist(language))
}
