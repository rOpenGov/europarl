#' Loads data from configuration file
#'
#' File should contains:
#' \itemize{
#'  \item dbname = "dbname"
#'  \item host = "host"
#'  \item username = "username"
#'  \item password = "password"
#' }
#'
#' @param name file name or path to file
#' @param delim a delim parametr in read_delim
#'
#' @return A tibble with dbname, host, username and password for database conncetion.
#'
#' @examples
#' read_config()
#' read_config(file = "path/name.txt", delim = " ")
#'
#' @export
#' @import tidyverse



read_config <- function(file = 'db_config.txt', delim = " ") {

  config <- read_delim(paste0('./', file), delim = delim,
                       col_names = FALSE)

  config <- config[,-2]
  colnames(config) <- c('name', 'value')
  return(config)
}

