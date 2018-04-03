

library(rvest)
library(RSelenium)
library(tidyverse)
library(RMySQL)
library(europarl)
library(data.table)
library(glue)
library(lubridate)

# code
# connect to database
config <- read_config()

db <- dbConnect(MySQL(),
  dbname = config$value[1],
  host = config$value[2],
  username = config$value[3],
  password = config$value[4],
  encoding = 'utf-8'
)
dbSendQuery(db,"SET NAMES 'utf8';")


deputies <-  get_all_deputies(8)
deputies <- deputies[[8]]

dep_in_db <- dbGetQuery(db,"SELECT DISTINCT id FROM deputies
                         LEFT JOIN term_of_office ON deputies.id = term_of_office.deputies_id
                         WHERE term_of_office.term = '8';")

colnames(deputies)[4] <- 'id'

new_dep <- anti_join(deputies, dep_in_db, by = 'id')

dep_to_update <- semi_join(deputies, dep_in_db, by = 'id')

# new deputies
{
  more_info <- lapply(seq_along(new_dep$link), function(x) {
    link <- dep$link[x]
    cat(sprintf('P%d %0.2f%%: %s \n',i, x/max*100, link))
    return(get_more_info(link))
  })
  more_info <- rbindlist(more_info)
  data <- cbind(new_dep, more_info)

  data <- data %>%
    select(-2)
  dbWriteTable(db, 'deputies', data,
               append = TRUE, row.names = FALSE)

term_of_office <-  new_dep %>%
    select(id) %>%
    mutate(term = i)
  colnames(term_of_office)[1] <- 'deputies_id'

dbWriteTable(db, 'term_of_office', term_of_office,
             append = TRUE, row.names = FALSE)
## eu_party etc.
eu_party <- data.frame()
national_party <- data.frame()
positions <- data.frame()
for(x in seq_along(new_dep$link))  {

  link <- new_dep$link[x]
  id <- new_dep$id[x]

  history <- deputies_get_history_of_services(link, id)
  eu_party <- bind_rows(eu_party, history[[1]])
  national_party <-  bind_rows(national_party, history[[2]])
  positions <-  bind_rows(positions, history[[3]])

}
history_of_service <- list(eu_group = eu_party, national_party =  national_party,
                           positions = positions)

eu_party <- history_of_service$eu_group


eu_party_short <- eu_party %>%
  mutate(temp_name = str_replace(name,'.* -',''))

eu_party_short$temp_name <- as.factor(eu_party_short$temp_name)
out <- levels(eu_party_short$temp_name)
out <- c('QQQ', out)
a <- collapse(out, sep ='.*| -')

eu_party_temp <- eu_party %>%
  mutate(full_name = str_replace_all(name,a,''))

eu_party_temp$full_name <- as.factor(eu_party_temp$full_name)

eu_party_temp$full_name <- as.factor(str_replace_all(
  eu_party_temp$full_name,'Non-attached Members','Non-attached'))

eu_party_temp <- eu_party_temp %>%
  mutate(position = str_replace(name,'.* -',''))

eu_party_temp$position <- as.factor(eu_party_temp$position)

data <- eu_party_temp
eu_temp <- data.frame(
  date_beginning = data$date_beginning,
  date_end = data$date_end,
  deputies_id = data$deputy_id,
  position = data$position,
  full_name = data$full_name,
  original_text = data$name
)

dbWriteTable(db, 'eu_party', eu_temp,
             append=TRUE , row.names = FALSE)

national_party <- history_of_service$national_party
temp_national <- data.frame(
  full_name = national_party$name,
  date_beginning = national_party$date_beginning,
  date_end = national_party$date_end,
  deputies_id = national_party$deputy_id
)

dbWriteTable(db, 'national_party', temp_national,
             append=TRUE , row.names = FALSE)
rm(temp_national)


webdriver <- rsDriver(port = 4445L,  browser='firefox')

browser <- webdriver$client

statements_P8 <- list()
i <- 8
for(x in seq_along(new_dep[1,c('id')])) {


  if (x == length(new_dep$id) + 1) {
    break
  }
  cat('n: ',x,'\n')
  print(start, '\n')
  id <- new_dep[x, c('id')]
  data <- statements_get_all_statements(id, browser)

  tmp_data <- data

  data$date <- as_date(data$date)
  data$startTime <- hms(data$startTime)
  data$endTime <- hms(data$endTime)

  data$duration <- time_length(data$duration)

  data_db <- data.frame(
    deputies_id = id,
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

  cat('END n: ',x,'deputy:',id,'\n')
}


tmp <- dbSendQuery(db,"")

}
# end new dep
# dep to update


webdriver <- rsDriver(port = 4445L,  browser='firefox')
browser <- webdriver$client

for(i in seq_along(dep_to_update)) {
  statements_update_statements(dep_to_update$id[i], browser, db = db)
}

## history
{


  for(x in seq_along(dep_to_update$link))  {

      eu_party <- data.frame()
      national_party <- data.frame()
      positions <- data.frame()

      link <- dep_to_update$link[x]
      id <- dep_to_update$id[x]

      history <- deputies_get_history_of_services(link, id)
      eu_party <- bind_rows(eu_party, history[[1]])
      national_party <-  bind_rows(national_party, history[[2]])
      positions <-  bind_rows(positions, history[[3]])


      history_of_service <- list(eu_group = eu_party, national_party =  national_party,
                                 positions = positions)


      eu_party <- history_of_service$eu_group


      eu_party_short <- eu_party %>%
        mutate(temp_name = str_replace(name,'.* -',''))

      eu_party_short$temp_name <- as.factor(eu_party_short$temp_name)
      out <- levels(eu_party_short$temp_name)
      out <- c('QQQ', out)
      a <- collapse(out, sep ='.*| -')

      eu_party_temp <- eu_party %>%
        mutate(full_name = str_replace_all(name,a,''))

      eu_party_temp$full_name <- as.factor(eu_party_temp$full_name)

      eu_party_temp$full_name <- as.factor(str_replace_all(
        eu_party_temp$full_name,'Non-attached Members','Non-attached'))

      eu_party_temp <- eu_party_temp %>%
        mutate(position = str_replace(name,'.* -',''))

      eu_party_temp$position <- as.factor(eu_party_temp$position)

      data <- eu_party_temp
      eu_temp <- data.frame(
        date_beginning = data$date_beginning,
        date_end = data$date_end,
        deputies_id = data$deputy_id,
        position = data$position,
        full_name = data$full_name,
        original_text = data$name
      )

      tmp <- dbGetQuery(db, paste0('SELECT * FROM eu_party
                      WHERE deputies_id =',id , ';', sep =''))
      tmp$date_beginning <- as_date(tmp$date_beginning)
      tmp$date_end <- as_date(tmp$date_end)
      update_eu <- anti_join(tmp, eu_temp, by = c('original_text', "date_end", "date_beginning"))

      update_new <- update_eu %>%
        filter(!is.na(date_end))
      for(i in seq_along(update_new$id)) {

        sql <- paste0("UPDATE eu_party SET date_end = ",update_new$date_end[i],
                      "WHERE original_text = ",update_new$original_text[i],
                      "AND deputies_id = ", update_new$id[i],
                      "AND date_beginning = ", update_new$date_beginning[i], ";", sep ='')
        dbSendQuery(db, sql)
      }

      to_db <- update_eu %>%
        filter(is.na(date_end))
      if(nrow(to_db) > 0) {
        dbWriteTable(db, 'eu_party', to_db,
                   append=TRUE , row.names = FALSE)
      }


    national_party <- history_of_service$national_party
    temp_national <- data.frame(
      full_name = national_party$name,
      date_beginning = national_party$date_beginning,
      date_end = national_party$date_end,
      deputies_id = national_party$deputy_id
    )

    tmp <- dbGetQuery(db, paste0('SELECT * FROM national_party
                      WHERE deputies_id =',id , ';', sep =''))
      tmp$date_beginning <- as_date(tmp$date_beginning)
      tmp$date_end <- as_date(tmp$date_end)
      update_national <- anti_join(tmp, eu_temp, by = c('original_text', "date_end", "date_beginning"))

      update_new <- update_national %>%
        filter(!is.na(date_end))
      for(i in seq_along(update_new)) {
        sql <- paste0("UPDATE eu_party SET date_end = ",update_new$date_end[i],
                      "WHERE original_text = ",update_new$original_text[i],
                      "AND deputies_id = ", update_new$id[i],
                      "AND date_beginning = ", update_new$date_beginning[i], ";", sep ='')
        dbSendQuery(db, sql)
      }

      to_db <- update_national %>%
        filter(is.na(date_end))
      if(nrow(to_db) > 0) {
        dbWriteTable(db, 'eu_party', to_db,
                   append=TRUE , row.names = FALSE)
      }

  }
}
# END
dbDisconnect(db)

