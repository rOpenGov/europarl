#' Get time of statements
#'
#' @param url A url of statements
#' @return Returns duration, start time and end time of statement
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium
#' @import lubridate


time_of_statements <- function(url) {

  video_text <- 'Video of the speech'
  eu_url <- "http://www.europarl.europa.eu"

  page <- read_html(url)
  text <- page %>%
    html_nodes(xpath='//a')

  for(i in 1:length(text)) {
    title_video <- text[i] %>%  html_attr('title')
    if(!is.na(title_video)) {
      if(strings_identical(title_video,video_text)==TRUE) {
        video_url <- text[i] %>%  html_attr('href')
        error_page <- FALSE
        #porpawic
        tryCatch(page <- read_html(paste(eu_url,video_url,sep='/')),
                 error = function(e) {
                   error_page <<-  TRUE
                   return(error_page)
                 })
        ifelse(error_page == FALSE, {
          text <- page %>%
            html_nodes(xpath='//a')

          nodes <- page %>%
            html_nodes(xpath='//div[@class="info"]') %>%
            html_nodes(xpath='p')

          duration <- nodes %>%
            html_nodes(xpath='//p[@id="duration"]') %>%
            html_text()

          startTime <- nodes %>%
            html_nodes(xpath='//p[@id="startTime"]') %>%
            html_text()

          endTime <- nodes %>%
            html_nodes(xpath='//p[@id="endTime"]') %>%
            html_text()

          values <- list(duration = hms(duration), startTime = hms(startTime), endTime = hms(endTime))

          return(values)
        }, {
          cat('ok')
          return(list(duration = 'error', startTime = 'error', endTime = 'error'))
        })
      }
    }
  }
  return(list(duration = NA, startTime = NA, endTime = NA))

}


#' Remove white signs and comapre two strings
#'
#'
#' @return Returns TRUE or FALSE
#'
#'



strings_identical <- function(x,y) {
  #removes white signs and compare
  return(grepl(gsub('\\s+','', x),gsub('\\s+','', y)))
}
