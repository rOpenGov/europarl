
# testing script

library(rvest)
library(RSelenium)
library(XML)
library(data.table)

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
dep <- cbind(deputies_P8,more_info)

#=======================================
# get parties, positions
# page <- read_html(deputies_P8[12,c("link")])
# history <- get_history(deputies_P8[12,c("link")],deputies_P8[12,c("ID_deputy")])

rm(eu_party)
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



#=======================================
a <- lapply(deputies_P8[,c("ID_deputy")],
            function(x) {
              get_statements(x, browser)
            })
statements_P8 <- rbindlist(a)
save(statements_P8, deputies_P8, file="deputies_statements.rda")

data <- subset(statements_P8, lang == "el", select=c("title","text","date"))
