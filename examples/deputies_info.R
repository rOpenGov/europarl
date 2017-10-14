
# testing script

library(rvest)
library(RSelenium)
library(XML)
library(data.table)
library(devtools)


#start Rselenium server
webdriver <- rsDriver(port = 4445L, browser="firefox")
browser <- webdriver$client

deputies <-  get_all_deputies()
#save separate data frames
for (i in seq(deputies)) {
  assign(paste("deputies_P", i, sep = ""), deputies[[i]])
}

# remove object with patern in name: rm(list = ls(pattern = "df"))
#=======================================
#get more info, nationality, date of birth, date of death, place of birth

(a <- get_more_info(deputies_P8[1,c("link")]))
dep <- deputies_P8[1,]
dep <- cbind(dep, a)
i <- 1
more_info <- lapply(deputies_P8[,c("link")], function(x){
  cat(i,"\n")
  i <<-  i + 1
  return(get_more_info(x))
})
more_info <- rbindlist(more_info)
deputies_P8 <- cbind(deputies_P8,more_info)
save(statements_P8, deputies_P8, file="./data/deputies_P8.rda")

#=======================================
# get parties, positions
# page <- read_html(deputies_P8[12,c("link")])
# history <- get_history(deputies_P8[12,c("link")],deputies_P8[12,c("ID_deputy")])

if(exists("eu_party")) rm(eu_party, national_party,positions)
i <- 1
history <- mapply( function(url,id) {

  history <- get_history(url,id)
     if(exists("eu_party")) {
      eu_party <<- rbind(eu_party, history[[1]])
      national_party <<- rbind(national_party, history[[2]])
      positions <<- rbind(positions, history[[3]])
    }
    else {
      eu_party <<- history[[1]]
      national_party <<- history[[2]]
      positions <<- history[[3]]
    }
  cat(i,"\n")
  i <<- i + 1

},deputies_P8[,c("link")],deputies_P8[,c("ID_deputy")])

history_dep <- list(eu_party=eu_party, national_party=national_party, positions=positions)
for (i in seq(history_dep)) {
  assign(paste(names(history_dep)[i], "_P8", sep = ""), history_dep[[i]])
}
save(statements_P8, deputies_P8, eu_party_P8, national_party_P8, positions_P8, file="./data/P8.rda")


#=======================================
# NA in date of birth and place of birth
dep <- deputies_P8[which(is.na(deputies_P8$date_of_birth) | is.na(deputies_P8$place_of_birth)),]

#=======================================


in_writing <- read.delim(file="./materials/in_writing_dictionary.txt", encoding ="UTF-8", sep=";")
h <- paste(in_writing$name, collapse="|")
statements_P8$text <- gsub("\n","", statements_P8$text)
statements_P8$in_writing <- grepl(h,statements_P8$text)


#=======================================
a <- lapply(deputies_P8[,c("ID_deputy")],
            function(x) {
              get_statements(x, browser)
            })
statements_P8 <- rbindlist(a)
save(statements_P8, deputies_P8, file="deputies_statements_P8.rda")

