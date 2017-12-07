#' Get all statements for P8
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium

get_statements <- function(deputy_id, browser) {

  deputy_id <- deputy_id
  cat("deputy id:", deputy_id,"\n")
  eu_url <- "http://www.europarl.europa.eu"
  eu_meps_url <- "http://www.europarl.europa.eu/meps/en"

  type <- "CRE"
  url <- paste(eu_meps_url,deputy_id,"seeall.html?type=",sep="/")
  statements_url <- paste(url,type, sep="")

  Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  #======================
  browser <- browser
  browser$navigate(statements_url)

  # check if there is buuton
  button <- browser$findElements(using="class", value="blue_button")

  button <- browser$findElement(value="//a[@class='blue_button']")
  while(button$isElementDisplayed()==TRUE) {
    button <- browser$findElement(value="//a[@class='blue_button']")
    button$highlightElement()

    button$clickElement()
    Sys.sleep(5)
    cat("button\n")
  }

  #get information:
  titles <- browser$findElements(value="//p")
  titles <- sapply(titles, function(x){x$getElementText()})
  cat("titles: ", length(titles),"\n")
  if(length(titles) > 0){
    links <- browser$findElements(value="//p//a")
    links <- sapply(links, function(x){x$getElementAttribute("href")})
    cat("links: ", length(links),"\n")
    reference <- browser$findElements(using="class", value = "reference")
    reference <- sapply(reference, function(x){x$getElementText()})
    cat("reference: ", length(reference),"\n")

    date <- browser$findElements(using="class", value = "date")
    date <- sapply(date, function(x){x$getElementText()})
    date <- gsub(".*:\n","",date) %>% as.Date("%d-%m-%Y")
    cat("date: ", length(date),"\n")

    statements <- as.data.frame(date)
    statements$title <- unlist(titles)
    statements$reference <- unlist(reference)
    statements$link <- unlist(links)

    tf <-  sapply(statements$title, function(x) grepl("debate",x))
    statements$is_debate <-  tf

    data <- sapply(statements$link, function(x) {
      page <- read_html(x)
      lan_on <- page %>%
        html_nodes(".selected") %>%
        html_text()
      lan_on <-  substr(lan_on,1,2)
      return(lan_on)
      }
      )

    text <- sapply(statements$link, function(x) {

      url <- unlist(x)
      page <- read_html(url)

      text <- page %>%
        html_nodes(xpath='//p[@class="contents"]') %>%
        html_text()
      text
      paste(text,collapse="\n")
    })

    statements$text <- text

    statements$lang <- data
    statements$id_deputy <- deputy_id

    rownames(statements) <- seq(1:length(statements$lang))
    return(statements)
  }

}
