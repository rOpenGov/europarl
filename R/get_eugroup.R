#' Get data about all deputies
#'
#' @export
#' @import magrittr
#'
#'


get_eurogroup <- function(deputy_id, date = Sys.Date()) {

    stopifnot(is.character(deputy_id))

    if(!exists("eu_party_P8")) {
      stop("Please load europarlament groups database first.")
    }
    eu_group <- eu_party_P8[which(eu_party_P8$date_beginning <= date &
                                  ifelse(is.na(eu_party_P8$date_end),TRUE, eu_party_P8$date_end >= date) &
                                  eu_party_P8$ID_deputy==deputy_id
                                  ),c("name")]
    if(length(eu_group) > 1) {
      warning("More than one possibility. Error in data set. Returns first possibility.")
      return(eu_group[1])
    }
    else {
      return(eu_group)
    }
    #statements_table$eugroup  <- gsub("-.*","",statements_table$eugroup)

}

