#select_fish.R
#creats subset of data containing all of the detections of a single subject.

require(dplyr)

select_fish <- function(data, fish){
    selected_fish <- data %>% 
                        filter(pittag == fish)
    return(selected_fish)
    }
