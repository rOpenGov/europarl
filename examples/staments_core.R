library(rvest)
library(RSelenium)
library(XML)
library(lubridate)

webdriver <- rsDriver(port = 4445L,  browser='firefox')

browser <- webdriver$client


deputy_id <- '124958'
a <- statements_get_all_statements(deputy_id, browser)

deputies <-  get_all_deputies()
for (i in seq(deputies)) {
  assign(paste("df", i, sep = ""), deputies[[i]])
}





options(timeout= 4000000)
start <- Sys.time()
i <- 1
a <- lapply(df8[i,c("ID_deputy")],
            function(x) {
              cat('n: ',i,'\n')

              data <- get_statements(x, browser)

              tmp_data <- data
              tmp_data$text <- gsub('\n','_eol', data$text)
              write.table(tmp_data, paste("./data/partial_results/",i,"_",x,".txt",sep=""), sep="|",
                          fileEncoding ='UTF-8')

              cat('n: ',i,'deputy:',x,'\n')
              i <<-  i + 1

              return(data)

            })
librprint(Sys.time()-start)


statements_P8 <- rbindlist(a)
save(statements_P8, deputies_P8, file="deputies_statements.rda")



