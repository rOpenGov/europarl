#' Get all statements for P8
#'
#' @export
#' @import rvest
#' @import magrittr
#' @import RSelenium

statements_get_all_statements <- function(deputy_id, browser, term_of_office = 8) {

  statements <- statements_get_list_of_statements(id, browser, term_of_office)
  if(nrow(statements) > 0 ) {
    max <- length(statements$link)
    statements_details <- lapply(seq_along(statements$link), function(x) {
          link_x <- statements$link[x]
          #cat(deputy_id,' statement:',count, ',url:',x,'\n')
          cat(sprintf('%s %0.2f%% statement: %s \n',deputy_id, x/max*100, link_x))
          values <- statements_get_statement(link_x)

          return(values)
        })
    statements_details <-  do.call('rbind', statements_details)
    statements <- statements %>% select(-link)
    statements <- cbind(statements, statements_details)

    statements$deputy_id <- deputy_id

    rownames(statements) <- seq(1:length(statements$deputy_id))
    return(statements)
  }

}
