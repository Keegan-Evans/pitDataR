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

    #create variables to facilitate grouping by resolution
    resolution_columns <- date_time_columns_selector(detection_resolution, data)
    resolution_column_value <- resolution_columns[[1]]
    date_column_names <- resolution_columns[[2]]


    #subset data to only specified days
    selectData <- subset_by_date_range(data, start_date, end_date)

    #group by resolution
    grouped_by_resolution <- selectData %>%
        group_by_at(.vars = date_column_names) %>%
        count(tag)

    return(grouped_by_resolution)


}

date_time_columns_selector <- function(resolution, data){
    if(resolution == "year"){
        reso_var <- (list(5, names(data)[5]))
    }else if(resolution == "month"){
        reso_var <- (list(6, names(data)[5:6]))
    }else if(resolution == "week"){
        reso_var <- (list(7, names(data)[5:7]))
    }else if(resolution == "day"){
        reso_var <- (list(8, names(data)[5:8]))
    }else if(resolution == "hour"){
        reso_var <- (list(9, names(data)[5:9]))
    }else{
        #better error handling here
        stop("The selected resolution is not an available interval.")
    }
    #output
    return(reso_var)

}
