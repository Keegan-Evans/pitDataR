#' only_tags_of_interest.R
#' subsets all of the data to include only the values from list of tags.
#' @param data the dataframe containing all detection data
#' @param tags the dataframe containing the tags of interest.


only_tags_of_interest <- function(data, tags){
    subsetted_data <- filter(data, tag %in% tags$tag)
    return(subsetted_data)
}
