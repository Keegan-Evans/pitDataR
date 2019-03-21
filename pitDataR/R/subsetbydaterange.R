#' subset_by_date_range
#' @param data_set The data set to get data from
#' @param date_col The column in the data set storing the date
#' of observation
#' @param start_date Enter as character string "mm-dd-yyyy"
#' @param end_date Enter as character string "mm-dd-yyyy"
#' @return A data frame subsetted to observations only inclusively
#' within specified range, but including all original columns
#' @export
#'
#' Subsets data by specified range.
#' Uses lazyeval::inter() to allow for variously named datetime columns


subset_by_date_range <- function(data_set, date_col = "detected_at", start_date, end_date, na.rm=FALSE){
    #convert character string start and end date to date class
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")


    filter_crit <- lazyeval::interp(~ as.Date(y) <= end &
                                        as.Date(y) >= start,
                                    .values=list(y = as.name(date_col),
                                                 start = start_date,
                                                 end = end_date))

    #subset data to only specified days
    selectedData <- data_set %>%
        filter_(filter_crit)

    return(selectedData)
}


