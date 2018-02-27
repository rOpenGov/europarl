directory <- './data/partial_results/'

deputies_P6 <- deputies[[6]] %>%
  mutate(row_n = rownames(deputies[[6]])) %>%
  select(name, id, row_n)

tmp <- read.delim(paste0(directory,'/6/',"6_1.txt"), sep="|", stringsAsFactors = FALSE)


statements_P6 <- tmp

statements_P6$text <- gsub('_eol','\n', statements_P6$text)
nazwy <- colnames(statements_P6)


data <- statements_P6


data$duration <- time_length(data$duration)

data_db <- data.frame(
  deputies_id = data$deputy_id,
  date = data$date,
  title = data$title,
  reference = data$reference,
  language_code = data$lang_selected,
  text = data$text,
  duration = data$duration,
  start_time = data$startTime,
  end_time = data$endTime,
  link = data$link,
  term = 6
)
dbWriteTable(db, 'statements', data_db,
             append = TRUE , row.names = FALSE)

# dbGetQuery(db, "SELECT text from statements
#            WHERE id = 1")

for(i in seq_along(deputies_P6$id)) {
  i <- i + 1
  cat(i, '\n')
  if(i == length(deputies_P8$id)) {
    break()
  }


  data <-  read.delim(paste0(directory,'/6/',"6_", i,".txt"), sep="|", stringsAsFactors = FALSE)


  data$text <- gsub('_eol','\n', data$text)
  cat(i,' ',deputies_P6$name[i] , 'id:',deputies_P6$id[i], '\n')
  #statements_P8_pl <- rbind(statements_P8_pl, data)

  if(nrow(data) != 0) {

    data$duration <- time_length(data$duration)

    data_db <- data.frame(
      deputies_id = data$deputy_id,
      date = data$date,
      title = data$title,
      reference = data$reference,
      language_code = data$lang_selected,
      text = data$text,
      duration = data$duration,
      start_time = data$date,
      end_time = data$date,
      link = data$link,
      term = 6
    )


    dbWriteTable(db, 'statements', data_db,
                 append=TRUE , row.names = FALSE)
  }
}

