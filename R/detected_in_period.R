#' detected_in_period.R This is a method to determine if there was a detection
#' of a particular tag in a specied period and return a bolean of detections
#'
#' @param detection_dataframe Data read in using
#'   \code{\link{get_detection_data}}.
#' @param start_date Enter as character string "MM-DD-YYYY"
#' @param end_date Enter as character string "MM-DD-YYYY"
#' @param known_tags Tags either imported using\code{\link{tags_from_db}}.
#' @export

detected_in_period <- function(data_set,
                               start_date = NULL,
                               end_date = NULL,
                               known_tags){
    #subset data to only specified days
    #subset by date range if specified
    if(is.null(start_date) == FALSE & is.null(end_date) == FALSE){
        data_set <- subset_by_date_range(data_set = data_set,
                                         start_date = start_date,
                                         end_date = end_date)
    }

    #group by resolution
    detected_in_period <- data_set %>%
                            mutate(tag = as.factor(tag)) %>%
                            group_by(tag) %>%
                            count(tag) %>%
                            mutate(detected = n > 1) %>%
                            select(tag, detected)

    return(detected_in_period)
}
