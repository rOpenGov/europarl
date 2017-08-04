#return subpage of give page & lnk
subpage <- function(url, link) {
  s <- html_session(url)
  s <- s %>% follow_link(link)
  url_1 <- s$url
  page <- read_html(url_1)
  return(page)
}

#return genders & current status
# change, current status out
get_gender_active <- function(name) {

  url <- "http://www.europarl.europa.eu/meps/pl/directory.html?filter=all&leg=0#"

  to_return <- c("","")

  # page <- subpage(url, name)
  link <- name
  s <- html_session(url)
  s <- s %>% follow_link(link)
  url_1 <- s$url
  page <- read_html(url_1)


  text <- page %>%
    html_nodes(".more_info") %>%
    html_text()
  text <- trimws(text) #clean text, to check! gsub

  text[1]
  if(text[1] == "") {
    to_return[1] <- "inactive"
    s <- s %>% follow_link("Przegląd wszystkich kadencji")
    page <- read_html(s$url)
    h <-  page %>%
      html_nodes("h4") %>%
      html_text()
    x <- match("Członek", h)
    if(!is.na(x))
      to_return[2] <- "Male"
    else
      to_return[2] <- "Female"

  }
  else if(text[1] == "Członek") {
    to_return[1] <- "active"
    to_return[2] <- "Male"
  }
  else if(text[1] == "Członkini") {
    to_return[1] <- "active"
    to_return[2] <- "Female"
  }
  else {
    to_return[1] <- "active"
    h <-  page %>%
      html_nodes("h4") %>%
      html_text()
    x <- match("Członek", h)
    if(!is.na(x))
      to_return[2] <- "Male"
    else
      to_return[2] <- "Female"
  }


  return(to_return)
}

#return id_deputy
get_id_deputy <- function(name) {

  url <- "http://www.europarl.europa.eu/meps/en/directory.html?filter=all&leg=0#"
  s <- html_session(url) #entry site
  s <- s %>% follow_link(name) #go to link -> name of deputy
  url_deputy <- s$url #get url of subpage
  x <- sub("/.*", "",sub(".*/en/", "", url_deputy)) #extarct id from url
  return(x)

}

