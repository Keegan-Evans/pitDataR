#' Movement direction detection
#'
#' Returns data on directional movement across detection array.
#'
#' @param dataset The data set containing the observations
#' @return Returns a dataframe containing the tag, detection time/date info,
#'   direction of travel, and which detector interval made the observation.
#' @export

movement_detection <- function(dataset){
    dir <- dataset %>%
        #look at tags individually
        group_by(tag) %>%
        #sort to allow for determing difference between antenna
        arrange(detected_at) %>%
        #add column to show travel direction
        mutate(direction = ifelse(c(0,diff(antenna))<0, "up",
                                  ifelse(c(0,diff(antenna))>0, "down",
                                         "N"))) %>%
        #determine which interval the detection occured in.
        mutate(detector_interval = ifelse(c(antenna == "02" & direction == "up" |
                                    antenna == "03" & direction == "down"),
                                 "downstream",
                                 ifelse(c(antenna == "01" & direction == "up"|
                                            antenna == "02" & direction == "down"),
                                        "upstream",
                                        "unknown"))) %>%
        #remove instances where the fish sat next to detector
        filter(direction != "N") %>%
        select(tag, detected_at, direction, detector_interval)
    return(as.data.frame(dir))
}
