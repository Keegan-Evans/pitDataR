#' StartEndToDates
#'
#' @param start_date Enter as character string "Month-Day-Year"
#' @param end_date Enter as character string "Month-Day-Year"
#'
#'

StartEndToDates <- function(start_date, end_date){
    #convert character string start and end date to date class
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")

    return(c(start_date, end_date))
}
