

#' add_all_tags
#'
#' This function creates a data frame from all of the possible tags that matches
#' the format of the detection data, then joins it with the detection data to
#' produce a table to be fed to MARK to determine capture probabilities.
#'
#' @return returns dataframe that contains subsetted master detection dat of
#'   containing only observations for speciemens from specified list of tags
#'   plus blank observations of ALL tags from list. This allows for
#'   initialization of dataframe in
#'   \code{\link{generate_data_for_survival_analysis}}
#' @export

add_all_tags <- function(detection, tags){

    #restrict detection data to only tags of interest
    detection <- only_tags_of_interest(detection, tags)

    #blank dataframe
    allTagsDF <- as.data.frame(matrix(NA, nrow = nrow(tags), ncol = 9))
    #change names to match
    detectionNames <- names(detection)
    colnames(allTagsDF) <- detectionNames

    #get all pit tags
    allTagsDF$tag <- tags$tag

    #combine the data
    allTagsDF <- rbind(detection, allTagsDF)

    return(allTagsDF)

}


#' only_tags_of_interest
#'
#' subsets all of the data to include only the values for specified tags.
#'
#'
#' @param data the dataframe containing all detection data
#' @param tags the dataframe containing the tags of interest.
#' @return Returns dataframe with the same variables as the original data set,
#'   but only containing observations for the selected tags.


only_tags_of_interest <- function(data, tags){
    subsetted_data <- dplyr::filter(data, tag %in% tags$tag)
    return(subsetted_data)
}
