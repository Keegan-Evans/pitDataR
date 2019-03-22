#' first_last
#'
#' @param data_set Detection data. Use \code{\link{get_detection_data}} to
#'   import data first.
#' @param start_date Optional. Use to specify beginning of observation range.
#' @param end_date Optional. Use to specify end of observation range.
#' @return Returns data frame with variables: bleh, ah, sing, crazy_hair
#' @export

first_last <- function(data_set, start_date = NULL, end_date = NULL){
    #subset by date range if specified
    if(is.null(start_date) == FALSE & is.null(end_date) == FALSE){
        data_set <- subset_by_date_range(data_set = data_set,
                                         start_date = start_date,
                                         end_date = end_date)
    }

    #
    first_last_df <- data_set %>%
        group_by(tag) %>%
        arrange(detected_at) %>%
        summarize(first_detection = as.character(as.Date(first(detected_at))),
                  last_detection = as.character(as.Date(last(detected_at)))
                  ) %>%
        select(tag, first_detection, last_detection)
    first_last_df <- as.data.frame(first_last_df)


}
