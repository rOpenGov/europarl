



get_statements(df8[212,c('ID_deputy')],browser)



url <- 'http://www.europarl.europa.eu/sides/getDoc.do?pubRef=-//EP//TEXT+CRE+20160526+ITEM-007-05+DOC+XML+V0//EN&language=en&query=INTERV&detail=2-277-750'


time_of_statements(url)

  page <- read_html(url)
  cat(deputy_id,' statement:',count, ',url:',x,'\n')
  lan_on <- page %>%
    html_nodes(".selected") %>%
    html_text()
  lan_on <-  substr(lan_on,1,2)

  text <- page %>%
    html_nodes(xpath='//p[@class="contents"]') %>%
    html_text()
  text <- as.character(paste(text,collapse="\n"))

  time <- time_of_statements(url)
  time <- as.data.frame(time)

  count <<- count + 1
  values <- data.frame(lan_on,text, time)
  return(values)

