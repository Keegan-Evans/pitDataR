#' detected_in_period_resolution
#'
#' This function allows you to detect which specimens from a list were detected
#' either over all time or over specified interval. Shows detections per
#' specified resolution intervals.
#'
#' @param detection_dataframe Data read in using
#'   \code{\link{get_detection_data}}.
#' @param start_date Enter as character string "MM-DD-YYYY"
#' @param end_date Enter as character string "MM-DD-YYYY"
#' @param known_tags Tags either imported using\code{\link{tags_from_db}}.
#' @param resolution The resolution you want to view the data divided into
#'   specified as a character string of one of the following: "year", "month",
#'   "week", "day", or "hour"
#' @export

detected_in_period_resolution <- function(data_set,
                               start_date = NULL,
                               end_date = NULL,
                               known_tags,
                               detection_resolution){

    #add species to data
    data_with_species <- dplyr::left_join(data_set, known_tags, by = "tag")
    #change datetimes to dates to allow subsetting by daterange
    data_with_species$detected_at <- as.Date(data_with_species$detected_at)

    #create variables to facilitate grouping by resolution
    resolution_columns <- date_time_columns_selector(detection_resolution, data_with_species)
    resolution_column_value <- resolution_columns[[1]]
    date_column_names <- resolution_columns[[2]]

    #subset data to only specified days if provided dates
    if(!is.null(start_date)| !is.null(end_date)){
    data_with_species <- subset_by_date_range(data_with_species, start_date, end_date)
    }


    #group by resolution
    grouped_by_resolution <- data_with_species %>%
        dplyr::group_by_at(.vars = date_column_names) %>%
        dplyr::count(tag)

    return(grouped_by_resolution)


}




#' gets column indexes of specified resolution to allow grouping for the
#' function detected_in_period_resolution
#' @param resolution The resolution you want to view the data divided into
#'   specified as a character string of one of the following: "year", "month",
#'   "week", "day", or "hour"
#' @param data The dataset you are wanting to group within.
#' @return Returns list of column names that allow grouping by resolution
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
