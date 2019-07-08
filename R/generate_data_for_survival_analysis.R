#' generate data for survival analysis
#'
#' Used for preparing data for survival analysis using MARK software.
#' @param data_set Data read in using \code{\link{get_detection_data}}.
#' @param tags Dataframe of tags to be examined
#' @param dates Character vector containing the dates at which you want to break
#'   the analysis into. Use "mm-dd-yyyy" format when entering dates.
#' @return Eventually the function will return a .txt file of the data formatted
#'   to the detection history format used by MARK software that will allow for
#'   survival analysis. Currently it creates a dataframe object containing
#'   columns for tag, a initial detection, and each subsequent detection period.
#'   The detection period data is recorded as a either a 0 for no detection in
#'   that period or as a 1 indicating a detection in that period.
#' @export
#' @note This generates data specifically for mark/recapture survivalship
#'   analysis. For more information see
#'   \href{http://www.phidot.org/software/mark/docs/book/}{\emph{Program MARK: a
#'   gentle introduction}}.
generate_data_for_survival_analysis <- function(data_set, tags, dates){
    #add all tags from list to data frame so initial will be TRUE
    alldata <- add_all_tags(data_set, tags)

    #intialize data frame with release tag detection to set to true
    all_periods <- data.frame(tag = tags$tag, initial = TRUE)


    #list to hold detections
    period_detections <- list()
    period_names <- list()

    #initialize vector to hold individual detection interval observations

    #loop through all dates, call detected_in_period on each interval
    # premise for iterating through character vector storing dates
    # for(i in 2:length(dcv)-1){
    #     +     print(paste(dcv[i], dcv[i + 1]))
    #     + }
    for(i in 2:length(dates) - 1){
        each_period <- detected_in_period(alldata, dates[i], dates[i + 1])
        all_periods <- dplyr::left_join(all_periods, each_period, by = c('tag' = 'tag'))
        period_names[[i]] <- paste(dates[i], "-", dates[i + 1])
    }

    colnames(all_periods) <- c("tag", "initial", period_names)



    # #NA -> FALSE
    all_periods <- all_periods %>%
        dplyr::mutate_at(.vars = c(3:ncol(all_periods)),
                         ~replace(., is.na(.), FALSE)
        )

    # #TRUE/FALSE to 0/1
    all_periods <- cbind(all_periods[,1], all_periods[ ,c(2:ncol(all_periods))] * 1)


    return(all_periods)

}

