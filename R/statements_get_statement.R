#' Get statements text and details.
#'
#' Function \code{statements_core} downloads content and details(language, time) of the statement.
#'
#'
#'
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium


statements_get_statement <- function(url) {

   page <- safe_html(url)


   lang_on <- page %>%
     html_nodes(".on") %>%
     html_text()

   time <- statements_get_time(url)
   time <- as.data.frame(time)

   text <- page %>%
     html_nodes(xpath='//p[@class="contents"]') %>%
     html_text()
   text <- as.character(paste(text,collapse="\n"))

   lang_selected <- page %>%
     html_nodes(".selected") %>%
     html_text()
   lang_selected <-  substr(lang_selected,1,2)

   if(length(lang_on) > 1) {
    lang_on <-  substr(lang_on,1,2)
    lang_on <- toupper(paste(lang_on, collapse = '\\)|\\('))
    lang_on <- paste0('\\(', lang_on, '\\)')
    tmp <- str_extract(text, lang_on) %>%
           str_replace('\\(', '')  %>%
            str_replace('\\)', '')
    if(is.na(tmp)) tmp <- lang_selected
    cat('\tlang:', tmp, '\n')
    url <- str_replace(url,'EN&language=en', tmp)
    page <- safe_html(url)
    lang_selected <- page %>%
      html_nodes(".selected") %>%
      html_text()
    lang_selected <-  substr(lang_selected,1,2)
    if(lang_selected == tolower(tmp)) {
      text <- page %>%
        html_nodes(xpath='//p[@class="contents"]') %>%
        html_text()
      text <- as.character(paste(text,collapse="\n"))
    }
   }


  values <- data.frame(lang_selected, text, time, link = url ,stringsAsFactors = FALSE)

  return(values)
}
