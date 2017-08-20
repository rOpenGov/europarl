library(rvest)
library(RSelenium)
library(XML)
library(data.table)


webdriver <- rsDriver(port = 4445L, browser="firefox")
browser <- webdriver$client 

start <- Sys.time()
a <- lapply(deputies_P8[,c("ID_deputy")],
            function(x) {
              get_statements(x, browser)
            })
print(Sys.time()-start)


statements_P8 <- rbindlist(a)
save(statements_P8, deputies_P8, file="deputies_statements.rda")

data <- subset(statements_P8, lang == "el", select=c("title","text","date"))
