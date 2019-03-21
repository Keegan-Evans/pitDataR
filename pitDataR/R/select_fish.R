#select_fish.R
#creats subset of data containing all of the detections of a single subject.



select_fish <- function(data_set, fish){
    selected_fish <- data_set %>%
                        filter(tag == fish)
    return(selected_fish)
    }


only_specified_tags <- function(detection_data, tag_data){
    fish_detected <- detection_data %>%
        filter(tag %in% tag_data[['tag']])
    return(fish_detected)
}
