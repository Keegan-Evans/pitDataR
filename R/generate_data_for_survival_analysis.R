#' generate data for survival analysis
#'
#' Used for preparing data for survival analysis using MARK software.
#' @param data_set Data read in using
#'   \code{\link{get_detection_data}}.
#' @param tags Dataframe of tags to be examined
#' @param dates Character vector containing the dates at which you want to break
#'   the analysis into. Use "mm-dd-yyyy" format when entering dates.
#' @return
#' @export

generate_data_for_survival_analysis <- function(data_set, tags, dates){
    #add all tags from list to data frame so initial will be TRUE
    alldata <- add_all_tags(data_set, tags)

    #intialize data frame with release tag detection to set to true
    release <- data.frame(tag = tags$tag, intial = TRUE)

    #initialize vector to hold individual detection interval observations

    #loop through all dates, call detected_in_period on each interval
    # premise for iterating through character vector storing dates
    # for(i in 2:length(dcv)-1){
    #     +     print(paste(dcv[i], dcv[i + 1]))
    #     + }


    #initialize master frame by joining release to first interval
    #loop through individual intervals dplyr::full_joining them to master frame




}

