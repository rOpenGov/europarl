
library(tidyverse)

deputies_P8 <- deputies[[8]]

deputies_P8_pl <- deputies_P8 %>%
  mutate(row_n = rownames(deputies_P8)) %>%
  filter(nationality == 'Poland') %>%
  select(name, id, row_n)

directory <- './data/partial_results/'

statements_P8_pl <- read.table(paste(directory,'8_',deputies_P8_pl$row_n[1],'.txt', sep=''),
                               encoding ="UTF-8", sep='|', stringsAsFactors = FALSE)
statements_P8_pl$text <- gsub('_eol','\n', statements_P8_pl$text)
nazwy <- colnames(statements_P8_pl)

for(i in seq_along(deputies_P8_pl$id) - 1) {
  i <- i + 1
  k <- deputies_P8_pl$row_n[i]

  try_read <- try(data <- read.table(paste(direcotry,'8_', k,'.txt', sep=''),
                        encoding ="UTF-8", sep='|', stringsAsFactors = FALSE))
  if(class(try_read)[1] == 'try-error') {
    cat('error \n')
    tmp_data <- read.table(paste(direcotry,'8_', k,'.txt', sep=''),
                       encoding ="UTF-8", sep='|', stringsAsFactors = FALSE, quote="")

      for(colname in names(tmp_data)){
        tmp_data[[colname]] <- gsub('\"',"", tmp_data[[colname]])
      }

      #tmp_data[,7] <- gsub('_eol','\n', tmp_data[,7])

      colnames(tmp_data) <- nazwy
      data <- tmp_data
  }

  data$text <- gsub('_eol','\n', data$text)
  cat(k,' ',deputies_P8_pl$name[i] , 'id:',deputies_P8_pl$id[i], '\n')
  statements_P8_pl <- rbind(statements_P8_pl, data)
}
as_tibble(statements_P8_pl %>% filter(deputy_id=='28372'))

write.csv2(statements_P8_pl, paste0(directory,'/statements_pl.csv'), fileEncoding = 'UTF-8')

statements_P8_pl_inPL <- statements_P8_pl %>%
  filter(lang_on == 'pl')
write.csv2(statements_P8_pl_inPL, paste0(directory,'/statements_pl_inPL.csv'), fileEncoding = 'UTF-8')
