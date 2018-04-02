#' Get data about all deputies
#'
#' @export
#' @import magrittr
#'
#'


get_nationalparty <- function(deputy_id, date = Sys.Date()) {

  stopifnot(is.character(deputy_id))

  if(!exists("national_party_P8")) {
    stop("Please load national parties database first.")
  }
  national_party <- national_party_P8[which(national_party_P8$date_beginning <= date &
                                  ifelse(is.na(national_party_P8$date_end),TRUE, national_party_P8$date_end >= date) &
                                    national_party_P8$ID_deputy==deputy_id
  ),c("name")]

  return(national_party)

}
