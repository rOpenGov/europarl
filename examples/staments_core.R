library(rvest)
library(RSelenium)
library(XML)
library(lubridate)

webdriver <- rsDriver(port = 4445L, browser="firefox")
browser <- webdriver$client


deputy_id <- '28307'
get_statements(deputy_id, browser)

deputies <-  get_all_deputies()
for (i in seq(deputies)) {
  assign(paste("df", i, sep = ""), deputies[[i]])
}



start <- Sys.time()
i <- 1
a <- lapply(deputies_P8[1:100,c("ID_deputy")],
            function(x) {
              cat('n: ',i,'\n')
              i <<-  i + 1
              return(get_statements(x, browser))

            })
print(Sys.time()-start)


statements_P8 <- rbindlist(a)
save(statements_P8, deputies_P8, file="deputies_statements.rda")


