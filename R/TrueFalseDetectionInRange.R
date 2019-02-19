#' TrueFalseDetectionInRange
#'
#'

TrueFalseDetectionInRange <- function(data_set, start_date, end_date, tag_list){

    #formatted dates
    start_date <- as.Date(start_date, format = "%m-%d-%Y")
    end_date <- as.Date(end_date, format = "%m-%d-%Y")

    data_set <- subset_by_date_range(data_set, start_date, end_date)

    TFdetected <- data.frame(detected_at = as.POSIXct(character()),
                             tag = factor()
                             )
    colnames(TFdetected) <- c("detected_at", "tag")

    for(i in data_set$detected_at){
        for(tag in tag_list){
            TFdetected[nrow(TFdetected) + 1, ] = list(i, tag)
        }
    }
    return(TFdetected)

}
