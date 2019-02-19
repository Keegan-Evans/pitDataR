#' Select Data from date range
#' @param data_set The data set to get data from
#' @param start_date Enter as character string "mm-dd-yyyy"
#' @param end_date Enter as character string "mm-dd-yyyy"
#'
#' Subsets data by specified range.
#'


subset_by_date_range <- function(data_set, start_date, end_date){
    #convert character string start and end date to date class
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")

    #subset data to only specified days
    selectedData <- data_set %>%
        filter(detected_at >= start_date & detected_at <= end_date)

    return(selectedData)
}
