


deputies <- df8
colnames(deputies)[4] <- 'id'


more_info <- lapply(deputies$link, function(x){
  cat(x, '\n')
  return(get_more_info(x))
})

more_info <- rbindlist(more_info)
deputies_P8 <- cbind(deputies,more_info)

deputies_P8 <- deputies_P8 %>%
  select(-P8)


save(statements_P8, deputies_P8, file="./data/deputies_P8.rda")




in_writing <- read.delim(file="./materials/in_writing_dictionary.txt", encoding ="UTF-8", sep=";")
h <- paste(in_writing$name, collapse="|")
statements_P8$text <- gsub("\n","", statements_P8$text)
statements_P8$in_writing <- grepl(h,statements_P8$text)


#get history

# dodaj eugroup

i <- 1
eugroup  <- mapply(function(id,date) {
  cat(id," i: ",i,"\n")
  i <<- i + 1
  return(get_eurogroup(id,date))
 },deputies$id,deputies$date)

 statements_table$eugroup  <- gsub("-.*","",statements_table$eugroup)

 i <- 1
 statements_table$nationalparty  <- mapply(function(id,date) {
   cat(id," i: ",i,"\n")
   i <<- i + 1
   get_nationalparty(id,date)
 },statements_table$id_deputy,statements_table$date)
 statements_table$nationalparty  <- gsub("\\(.*","",statements_table$nationalparty)


lang <- get_languages()
languages <- data.frame(
  gsub("\\s-.*","", lang),
  gsub(".*-\\s","", lang)
)
colnames(languages) <- c("lang", "Language")
languages$Language <- as.character(languages$Language)
languages$lang <- as.character(languages$lang)
statements_table <- left_join(statements_table,languages, by="lang")


statements_table <- statements_table %>%
 left_join(frakcje, by = "eugroup")
#byl w nazwie  "
statements_table$short <- as.character(statements_table$short)
statements_table$nationalparty <- gsub('\"','',statements_table$nationalparty)


tmp <- lapply(statements_table$nationalparty, function(x) {

  if(nchar(x)<=2) {
    cat(x, ";\n")
    x <- gsub('-|c','NI',x)
    cat(x)
    return('NI')
  }
  else {
    return(x)
  }
})
statements_table$nationalparty <- unlist(tmp)

  statements_table$short <- sapply(statements_table$short, function(x) {

    if (str_detect(x, "Greens")) {
      cat(x, "\n")
      return('Greens-EFA')

    }
    else {
      return(x)
    }
  })

