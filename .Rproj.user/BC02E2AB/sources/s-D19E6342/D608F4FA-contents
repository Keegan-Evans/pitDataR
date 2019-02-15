#' Detected in Period
#'
#' This function allows you to detect which specimens from a list were detected in a specified period
#' @param start_date Enter as character string "Month-Day-Year"
#' @param end_date Enter as character string "Month-Day-Year"
#' detected_in_period()

detected_in_period <- function(data,
                               start_date = min(data[['detected_at']]),
                               end_date = max(data[['detected_at']]),
                               known_tags,
                               detection_resolution){

    #create variables that allow call datetime columns easily later
    resolution_columns,  <- date_time_columns_selector(detection_resolution)
    first_dt_variable <- as.name(paste(detection_resolution))

    #convert character string start and end date to date class
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")

    #subset data to only specified days
    selectedData <- data %>%
        filter(detected_at >= start_date & detected_at <= end_date) %>%
        filter(tag %in% known_tags[['tag']]) %>%
        group_by(paste(resolution_columns)) %>%
        select(tag, first_dt_variable:year)

    #group by selected
    # grouped_by_month <- alldata %>%
    #     +     select(month, tag) %>%
    #     +
    #     + group_by(month) %>%
    #     + distinct()
}

date_time_columns_selector <- function(resolution){
    if(resolution == year){
        return("year")
    }else if(resolution == "month"){
        return("year, month")
    }else if(resolution == "day"){
        return("year, month, day")
    }else if(resolution == "hour"){
        return("year, month, day, hour")
    }else{
        #better error handling here
        stop()
    }
}
