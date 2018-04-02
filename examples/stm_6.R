directory <- './data/partial_results/'

for( k in 6:7) {
  deputies_P <- deputies[[k]] %>%
    mutate(row_n = rownames(deputies[[k]])) %>%
    select(name, id, row_n)

  tmp <- read.delim(paste0(directory,'/',k,'/',k,"_1.txt"), sep="|", stringsAsFactors = FALSE)


  statements_P <- tmp

  statements_P$text <- gsub('_eol','\n', statements_P$text)
  nazwy <- colnames(statements_P)


  data <- statements_P

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
    term = k
  )
  dbWriteTable(db, 'statements', data_db,
               append = TRUE , row.names = FALSE)

  # dbGetQuery(db, "SELECT text from statements
  #            WHERE id = 1")

  for(i in seq_along(deputies_P$id)) {
    i <- i + 685
    cat(i, '\n')
    if(i == length(deputies_P$id)) {
      break()
    }

    if(k == 7 & i >= 513) {
      data <- read.delim(paste0(directory,'/',k,'/',k,"_", i,".csv"), sep="|", stringsAsFactors = FALSE)
    } else {
      data <-  read.delim(paste0(directory,'/',k,'/',k,"_", i,".txt"), sep="|", stringsAsFactors = FALSE)
    }



    data$text <- gsub('_eol','\n', data$text)
    cat(i,' ',deputies_P$name[i] , 'id:',deputies_P$id[i], '\n')
    #statements_P8_pl <- rbind(statements_P8_pl, data)

    if(nrow(data) != 0) {

      data$date <- as_date(data$date)
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
        term = k
      )


      dbWriteTable(db, 'statements', data_db,
                   append=TRUE , row.names = FALSE)
    }
  }

}
