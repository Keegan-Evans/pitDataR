#' only_tags_of_interest subsets all of the data to include only the values of
#' from
#' @param data the dataframe containing all detection data
#' @param tags the dataframe containing the tags of interest.
#' @return Returns dataframe with the same variables as the original data set,
#'   but only containing observations for the selected tags.


only_tags_of_interest <- function(data, tags){
    subsetted_data <- filter(data, tag %in% tags$tag)
    return(subsetted_data)
}
