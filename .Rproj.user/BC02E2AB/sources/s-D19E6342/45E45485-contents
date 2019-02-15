#' group_by_resolution
#'
#' Groups data frame by resolution, orders by date
#' @param data -The dataset that you want to get the resolution grouping of
#' @param resolution -The resolution you want to group the data by
#'
group_by_resolution <- function(data, resolution){
    require(dplyr)
    grouped_data <- data %>%
        group_by(paste(resolution))

    return(grouped_data)
}
