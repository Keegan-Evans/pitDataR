#select_fish.R
#creats subset of data containing all of the detections of a single subject.

require(dplyr)

select_fish <- function(data, fish){
    selected_fish <- data %>%
                        filter(pittag == fish)
    return(selected_fish)
    }


fish_detected_from_list <- function(detection_data, tag_data){
    fish_detected <- detection_data %>%
        filter(tag %in% tag_data[['tag']])
    return(fish_detected)
}
