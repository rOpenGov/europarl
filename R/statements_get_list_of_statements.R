#' Return links, titles, dates of sttamenets of given deputie.
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium

statements_get_list_of_statements <- function(deputy_id, browser, term_of_office = 8) {


  cat("deputy id:", deputy_id,"\n")
  eu_url <- "http://www.europarl.europa.eu"
  eu_meps_url <- "http://www.europarl.europa.eu/meps/en"
  Sys.setlocale("LC_TIME", "C")


  type <- "CRE"
  url <- paste(eu_meps_url, deputy_id, "seeall.html?type=", sep="/")
  statements_url <- paste(url, type, "&leg=", term_of_office, sep="")

  #======================
  browser <- browser
  cat('url: ' ,statements_url, '\n')
  browser_try <- try(browser$navigate(statements_url))

  if(class(browser_try)[1] == "try-error") {
    cat('error\n')
    webdriver <- rsDriver(port = 4445L,  browser='firefox')
    browser <- webdriver$client
    Sys.sleep(1)
    browser$navigate(statements_url)
  }
  # check if there is buuton

  button_try <- try(button <- browser$findElement(value="//a[@class='blue_button']"), silent = TRUE)

  count <- 1

  if(class(button_try)[1] != "try-error") {
    button <- browser$findElement(value="//a[@class='blue_button']")
    while(button$isElementDisplayed()==TRUE) {

      button_try <- try(button <- browser$findElement(value="//a[@class='blue_button']"), silent = TRUE)
      if (class(button_try)[1] == "try-error") {
        Sys.sleep(15)
      }
      else {
        if(button$isElementDisplayed()==TRUE) {
          button$clickElement()
        }
      }
      Sys.sleep(4)
      cat(count,"button\n")
      count <- count + 1
    }

  }
  Sys.sleep(2)
  #get information:
  titles <- browser$findElements(value='//p[@class="title"]')
  titles <- sapply(titles, function(x){x$getElementText()})
  cat("titles: ", length(titles),"\n")
  if(length(titles) > 0){
    links <- browser$findElements(value='//p[@class="title"]//a')
    links <- sapply(links, function(x){x$getElementAttribute("href")})
    cat("links: ", length(links),"\n")
    reference <- browser$findElements(using="class", value = "reference")
    reference <- sapply(reference, function(x){x$getElementText()})
    cat("reference: ", length(reference),"\n")

    date <- browser$findElements(using="class", value = "date")
    date <- sapply(date, function(x){x$getElementText()})
    date <- gsub(".*:\n","",date) %>% as.Date("%d-%m-%Y")
    cat("date: ", length(date),"\n")

    if(length(titles) != length(date)) {
      titles <- browser$findElements(value='//p[@class="title"]')
      titles <- sapply(titles, function(x){x$getElementText()})
      links <- browser$findElements(value='//p[@class="title"]//a')
      links <- sapply(links, function(x){x$getElementAttribute("href")})
      reference <- browser$findElements(using="class", value = "reference")
      reference <- sapply(reference, function(x){x$getElementText()})
      date <- browser$findElements(using="class", value = "date")
      date <- sapply(date, function(x){x$getElementText()})
      date <- gsub(".*:\n","",date) %>% as.Date("%d-%m-%Y")

    }

    statements <- as.data.frame(date, stringsAsFactors = FALSE)
    statements$title <- unlist(titles)
    statements$reference <- unlist(reference)
    statements$link <- unlist(links)

    tf <-  sapply(statements$title, function(x) grepl("debate",x))

    statements$is_debate <-  tf


    return(statements)
  }

}
