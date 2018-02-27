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


statements_core <- function(url) {
  page <- safe_html(url)

  lan_on <- page %>%
    html_nodes(".selected") %>%
    html_text()
  lan_on <-  substr(lan_on,1,2)

  text <- page %>%
    html_nodes(xpath='//p[@class="contents"]') %>%
    html_text()
  text <- as.character(paste(text,collapse="\n"))

  time <- time_of_statements(x)
  time <- as.data.frame(time)


  values <- data.frame(lan_on,text, time)
  return(values)
}
