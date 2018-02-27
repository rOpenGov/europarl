#' Get nationality, date of birth, place of birth and/or date of death
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium

get_more_info <- function(home_page) {

  Sys.getlocale("LC_TIME")
  Sys.setlocale("LC_TIME", "C")

  page <- safe_html(home_page)

  text <- page %>%
    html_nodes(".more_info") %>%
    html_text()
  text <- trimws(text)

  i <- grep("birth",text)
  if(length(i) > 0) {
    if(length(grep(",",text)) > 0) place_of_birth <- trimws(sub(".*,","", text[i]))
    else place_of_birth <- NA
    birth <- trimws(sub(".*:","",sub(",.*","", text[i])))
  }
  else {
    place_of_birth <- NA
    birth <- NA
  }
  birth <- as.Date(birth, format ="%d %B %Y")

  i <- grep("death",text)
  if(length(i) > 0) {
    death <- trimws(sub(".*:","",sub(",.*","", text[i])))
  }
  else {
    death <- NA
  }
  death <- as.Date(death, format ="%d %B %Y")

  nationality <- page %>%
    html_nodes(xpath='//li[@class="nationality noflag"]') %>%
    html_text()
  nationality <- gsub("\r.*","", nationality)

  info <- list(nationality = nationality, date_of_birth = birth, place_of_birth = place_of_birth, date_of_death = death)

  return(info)

}

