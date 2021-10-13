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
#' \dontrun{
#' read_config()
#'
#' read_config(file = "path/name.txt", delim = " ")
#' }
#' @export
#' @importFrom readr read_delim
#' @importFrom dplyr filter
#' @importFrom dplyr anti_join
#' @importFrom dplyr select
#' @importFrom DBI dbGetQuery
#' @import tidyverse



read_config <- function(file = system.file("config/db_config.txt",
                          package = "europarl"
                        ),
                        delim = " ") {
  config <- read_delim(file,
    delim = delim,
    col_names = FALSE
  )

  config <- config[, -2]
  colnames(config) <- c("name", "value")
  return(config)
}
