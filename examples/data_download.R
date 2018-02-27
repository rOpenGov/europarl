library(RMySQL)
library(dplyr)
library(data.table)
library(dplyr)
library(rvest)
library(RSelenium)
library(europarl)



deputies <-  get_all_deputies()

for(i in seq_along(deputies)) {
  cat(str(i), ' ', class(i), '\n')
  colnames(deputies[[i]])[4] <- 'id'
  names(deputies)[i] <- paste0('P',i)
}


more_info_all <- list()
for(i in seq_along(deputies)) {

  cat(i, '\n')
  dep <- deputies[[i]]
  max <- length(dep$link)
  more_info <- lapply(seq_along(dep$link), function(x) {
     link <- dep$link[x]
     cat(sprintf('P%d %0.2f%%: %s \n',i, x/max*100, link))
     return(get_more_info(link))
  })
  more_info_all[[i]] <- rbindlist(more_info)
  deputies[[i]] <- cbind(dep,more_info_all[[i]])

  names(more_info_all)[i] <- paste0('P',i)
  closeAllConnections()
}

term_of_office_all  <-  list()
for (i in seq_along(deputies)) {
  term_of_office <- deputies[[i]] %>%
    select(id) %>%
    mutate(term = i)
  colnames(term_of_office)[1] <- 'deputies_id'
  term_of_office_all[[i]] <- term_of_office
  names(term_of_office_all)[i] <- paste0('P',i)
}

save(deputies, term_of_office_all, file = "./data/deputies_basic_info.Rda")
# statements

dep <- deputies[[1]] %>%
  select(id, link)

for(i in 2:8) {
  temp <- deputies[[i]] %>%
    select(id, link)
  dep <- union(dep, temp)
}



#======================


dep_list <- data.frame()
for(i in seq_along(deputies)) {
 dep <- deputies[[i]] %>%
    select(id, link)
  dep_list <- bind_rows(dep_list, dep)
}


dep_list <- dep_list %>%
    distinct(id, link)


eu_party <- data.frame()
national_party <- data.frame()
positions <- data.frame()
for(x in seq_along(dep_list$link))  {

  link <- dep_list$link[x]
  id <- dep_list$id[x]

  cat(sprintf('%0.2f%%: %s \n',x/max*100, link))

  history <- deputies_get_history_of_services(link, id)
  eu_party <- bind_rows(eu_party, history[[1]])
  national_party <-  bind_rows(national_party, history[[2]])
  positions <-  bind_rows(positions, history[[3]])

}
history_of_service <- list(eu_group = eu_party, national_party =  national_party,
                           positions = positions)
save(deputies, term_of_office_all, history_of_service ,file = "./data/deputies_info.Rda")

load(file = "./data/deputies_info.Rda")
# =============================

#sink('./examples/log.txt', split = TRUE, type = c("output", "message"))

webdriver <- rsDriver(port = 4445L,  browser='firefox')

browser <- webdriver$client

statements_P8 <- list()
i <- 8
for(x in seq_along(deputies[[8]][,c('id')])) {
  start <- Sys.time()
  x <- x + 516
  if (x == length(deputies[[8]]$id) + 1) {
    break
  }
  cat('n: ',x,'\n')
  print(start, '\n')
  id <- deputies[[8]][x, c('id')]
  data <- statements_get_all_statements(id, browser)

  tmp_data <- data
  tmp_data$text <- gsub('\n','_eol', data$text)
  #write.table(tmp_data, paste("./data/partial_results/",i,"_",x,".txt",sep=""), sep="|",
   #           fileEncoding ='UTF-8')
  write.csv(tmp_data, file =paste("./data/partial_results/",i,'/',i,"_",x,".csv",sep=""),
            fileEncoding ='UTF-8')
  #statements_P8[[x]] <- data
  end <- Sys.time()
  cat('END n: ',x,'deputy:',id,'\n')

}

webdriver <- rsDriver(port = 4445L,  browser='firefox')

browser <- webdriver$client

for (term in 7 ) {
  i <- term
  for(x in seq_along(deputies[[term]][,c('id')])) {
    start <- Sys.time()
    x <- x + 512
    if (x == length(deputies[[term]]$id) + 1) {
      break
    }
    cat('n: ',x,'\n')
    print(start, '\n')
    id <- deputies[[term]][x, c('id')]
    data <- statements_get_all_statements(id, browser, term_of_office = term)


    data$startTime <- data$startTime + data$date
    data$endTime <- data$endTime + data$date

    tmp_data <- data
    tmp_data$text <- gsub('\n','_eol', data$text)
    write.table(tmp_data, paste("./data/partial_results/",term,'/',term,"_",x,".csv",sep=""), sep="|",
                fileEncoding ='UTF-8')

    #statements_P8[[x]] <- data
    end <- Sys.time()
    cat('END n: ',x,'deputy:',id,'\n')

  }
}




