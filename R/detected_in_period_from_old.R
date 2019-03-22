#' detected_in_period.R
#' This is a method to determine if there was a detection of a particular tag
#' in a specied period and return a bolean of detections
#'
#' @export

detected_in_period <- function(data,
                               start_date = min(data[['detected_at']]),
                               end_date = max(data[['detected_at']]),
                               known_tags){
    #subset data to only specified days
    selectData <- subset_by_date_range(data, start_date, end_date)

    #group by resolution
    detected_in_period <- selectData %>%
                            mutate(tag = as.factor(tag)) %>%
                            group_by(tag) %>%
                            count(tag) %>%
                            mutate(detected = n > 1) %>%
                            select(tag, detected)

    return(detected_in_period)
}
