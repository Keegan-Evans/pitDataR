#' add_all_tags.R
#' This function creates a data frame from all of the possible tags that matches
#' the format of the detection data, then joins it with the detection data
#' to produce a table to be fed to MARK to determine capture probabilities.
#'

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
