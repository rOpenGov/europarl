#' Update stamenets in data abse
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium

statements_update_statements <- function(deputy_id, browser, term_of_office = 8, db) {

  sql <- paste0('SELECT link, date FROM statements
            inner join (
                SELECT max(date) max_date FROM statements
                WHERE deputies_id =',id,') dmax
                ON statements.date = dmax.max_date
                WHERE statements.date = dmax.max_date
                AND  deputies_id =',id,'
                ;', sep='')
  stm_in_db  <- dbGetQuery(db, sql)

  stm_in_db$date <- as_date(stm_in_db$date)

  tmp <- statements_get_list_of_statements(id, browser )

  tmp <- tmp %>%
    filter(date >= max(stm_in_db$date))

  to_download <- anti_join(tmp, stm_in_db, by ='link')

  if(nrow(to_download) != 0) {
    statements <- to_download
    max <- length(statements$link)
      statements_details <- lapply(seq_along(statements$link), function(x) {
        link_x <- statements$link[x]
        #cat(deputy_id,' statement:',count, ',url:',x,'\n')
        cat(sprintf('%s %0.2f%% statement: %s \n',deputy_id, x/max*100, link_x))
        values <- statements_get_statement(link_x)

        return(values)
      })
      statements_details <-  do.call('rbind', statements_details)
      statements <- statements %>% select(-link)
      statements <- cbind(statements, statements_details)

      statements$deputy_id <- deputy_id
      data <- statements

      data$date <- as_date(data$date)
      data$startTime <- hms(data$startTime)
      data$endTime <- hms(data$endTime)

      data$duration <- time_length(data$duration)

      data_db <- data.frame(
        deputies_id = data$deputy_id,
        date = data$date,
        title = data$title,
        reference = data$reference,
        language_code = data$lang_selected,
        text = data$text,
        duration = data$duration,
        start_time = data$date + data$startTime,
        end_time = data$date + data$endTime,
        link = data$link,
        term = 8
      )
      dbWriteTable(db, 'statements', data_db,
                    append = TRUE , row.names = FALSE)

  }
  return(invisible(NULL))
}
