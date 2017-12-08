

url <- 'http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20141020+ITEM-018+DOC+XML+V0//EN&language=en&query=INTERV&detail=1-056-000'
url2 <- 'http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20160120+ITEM-016+DOC+XML+V0//EN&language=en&query=INTERV&detail=3-743-000'
url3 <- 'http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20170613+ITEM-012+DOC+XML+V0//EN&language=en&query=INTERV&detail=2-567-000'
page <- read_html(url2)
text <- page %>%
  html_nodes(xpath='//a')

img <- page %>%
  html_nodes(xpath='//a[@title')


text[7] %>%  html_attr('href')
text[8] %>%  html_attr('href')
text[9] %>%  html_attr('href')
text[14] %>%  html_attr('href')

a <- text[14] %>%  html_attr('title')
b <- 'Video of the speech'

strings_identical <- function(x,y) {
  #removes white signs and compare
  return(grepl(gsub('\\s+','', x),gsub('\\s+','', y)))
}


title_video <- text[7] %>%  html_attr('title')

a <- time_of_statements('http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20141020+ITEM-018+DOC+XML+V0//EN&language=en&query=INTERV&detail=1-056-000')



#as.difftime(duration)


x <- 'http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20171129+ITEM-022+DOC+XML+V0//EN&language=en&query=INTERV&detail=1-262-000'

data <- lapply(statements_P8$link[1:2], function(x) {
  page <- read_html(x)
  lan_on <- page %>%
    html_nodes(".selected") %>%
    html_text()
  lan_on <-  substr(lan_on,1,2)

  text <- page %>%
    html_nodes(xpath='//p[@class="contents"]') %>%
    html_text()
  text <- paste(text,collapse="\n")
  time <- time_of_statements(x)

  time <- as.data.frame(time)


  values <- data.frame(lan_on,text, time)
  return(values)
}
)

tmp <- do.call("rbind", data)

text <- sapply(statements$link, function(x) {

  url <- unlist(x)
  page <- read_html(url)

  text <- page %>%
    html_nodes(xpath='//p[@class="contents"]') %>%
    html_text()
  text
  paste(text,collapse="\n")
})

time <- sapply(statements$link, function(x) {
  url <- unlist(x)
  time_of_statements(url)
})

b <- a$values[1]
t <- hms('00:01:41')
a <- as.difftime(duration, )

