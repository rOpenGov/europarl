#' Get data about all deputies
#'
#' @export
#' @import rvest
#' @import magrittr
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
      html_nodes(".mep_name") %>%
      html_text()
    deputies <- as.data.frame(deputies)
    deputies[] <- lapply(deputies, as.character)
    deputies$i <- 1
    colnames(deputies)[2] <- paste("P", i, sep ="")
    list_deputies[[i]] <- deputies
    i <- i + 1
  })
  # give current term of office
  list_deputies
}




