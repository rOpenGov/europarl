#' Get data about all deputies
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium
get_all_deputies <- function() {
  # repeat: get current term of office and create data_frame with deputies names
  i <- 1
  list_deputies <- list()
  repeat ({

    url <- paste("http://www.europarl.europa.eu/meps/en/directory.html?filter=all&leg=",i, sep = "")

    page <- read_html(url)

    number_results <- page %>%
      html_node(".blue") %>%
      html_text()

    cat(number_results, " ", i, "\n")
    number_results <-  gsub("[^0-9]","", number_results)
    if (number_results == 0)
      return(list_deputies)

    deputies <- page %>%
      html_nodes(".mep_name")
    #links
    href <- deputies %>%  html_nodes(xpath = "./a") %>%
      html_attr("href")
    href <- sapply(href, function(x) {
      x <- paste("http://www.europarl.europa.eu",x,sep="")
    })

    deputies <- as.data.frame(deputies %>% html_text())

    deputies[] <- lapply(deputies, as.character)
    deputies$i <- 1
    deputies$links <- href

    colnames(deputies) <- c("name",paste("P", i, sep =""), "link")

    deputies$ID_deputy <- sapply(deputies$link, function(x)
      sub("/.*", "",sub(".*/en/", "", x)) #extarct id from url
    )
    #assign deputies to deputies_parlamentary..
    assign(paste("deputies_P", i, sep= "" ),deputies)
    list_deputies[i] <- list(assign(paste("deputies_P", i, sep= "" ),deputies))
    i <- i + 1
  })
}


