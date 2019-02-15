#' Detected in Period
#'
#' This function allows you to detect which specimens from a list were detected in a specified period
#' @param start_date Enter as character string "Month-Day-Year"
#' @param end_date Enter as character string "Month-Day-Year"
#' detected_in_period()

detected_in_period <- function(data, start_date, end_date, known_tags = "NULL", resolution = "NULL"){
    require(dplyr)
    require(lubridate)

    #convert character string start and end date to date class
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")

    #subset data to only specified days
    selectedData <- data %>%
        filter(detected_at >= start_date & detected_at <= end_date) #%>%
        #filter(tag %in% known_tags$tag)

    #group by selected




}
