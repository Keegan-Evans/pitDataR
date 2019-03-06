#' detected_in_period_resolution.R
#'
#' This function allows you to detect which specimens from a list were detected in
#' a specified period divided by a specified resolution
#' @param start_date Enter as character string "Month-Day-Year"
#' @param end_date Enter as character string "Month-Day-Year"
#' @param resolution The resolution you want to view the data divided into,
#' specified as a character string of one of the folling: "year", "month", "week", "day"
#' detected_in_period()

detected_in_period_resolution <- function(data_set,
                               start_date = NULL,
                               end_date = NULL,
                               known_tags,
                               detection_resolution){


    data_with_species <- left_join(data_set, known_tags)
    data_with_species$detected_at <- as.Date(data_with_species$detected_at)

    #create variables to facilitate grouping by resolution
    resolution_columns <- date_time_columns_selector(detection_resolution, data_with_species)
    resolution_column_value <- resolution_columns[[1]]
    date_column_names <- resolution_columns[[2]]

    #subset data to only specified days
    if(!is.null(start_date)| !is.null(end_date)){
    data_with_species <- subset_by_date_range(data_with_species, start_date, end_date)
    }


    #group by resolution
    grouped_by_resolution <- data_with_species %>%
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
