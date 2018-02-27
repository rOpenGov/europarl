#' Get national parties, eu groups, postions
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import XML
#'



deputies_get_history_of_services <- function(home_page, deputy_id) {



  Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")
  eu_meps_url <- "http://www.europarl.europa.eu/meps/en"

  page <- safe_html(home_page)

  history <- page %>%
    html_node(xpath = '//ul[(@class = "tabs")]')
  hh <- htmlParse(history, asText=T)
  info <- xpathSApply(hh, "//a", xmlGetAttr, 'href')


  i <- unlist(grep(pattern ='history',info))
  history_url <- info[i]
  page <- read_html(paste(eu_meps_url, deputy_id, history_url,sep="/"))

  sections <- html_nodes(page, xpath = '//div[(@class = "boxcontent nobackground")]')

  hh <- htmlParse(sections, asText = TRUE, encoding="UTF-8")
  titles <- xpathSApply(hh, "//h4", xmlValue)
  info <- xpathApply(hh, "//ul", function(x)
    gsub("[\r\t\n]","",xpathSApply(x,"./li/text()", xmlValue)))
  info <- lapply(info,function(x){
    if(!length(x)) {
      x <- NA
    }
    x
  })
  #merge results together
  table <- do.call(rbind, Map(cbind, titles, info))
  content <- as.data.frame(trimws(table))

  content$name <- sapply(content[,2],function(x) gsub(".*:","",x))
  dates <- sapply(content[,2],function(x) gsub(":.*","",x))
  content$date_beginning <- trimws(substring(dates, 1, 10))
  content$date_end <- trimws(gsub("/","",substring(dates,11,nchar(dates))))

  content$date_beginning <- as.Date(content$date_beginning, format ="%d.%m.%Y")
  content$date_end <- as.Date(content$date_end, format ="%d.%m.%Y")

  political_group <- content[c("Political groups"),]

  colnames(content)[1] <- "position"
  content$deputy_id <- deputy_id
  #
  # #get EU parties
  eu_party <- content[which(content$position=="Political groups"),3:6]
  #
  # #get national parties
  national_party <- content[which(content$position=="National parties"),3:6]

  history <- list(eu_party = eu_party, national_party = national_party, positions =content[which(content$position!="Political groups" & content$position!="National parties"),-2] )
  return(history)


}
