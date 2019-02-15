#first_last.R
#returns first and last detection of given fish in specified period
first_last <- function(
    data, 
    start_date = which.min(data$detected_at), 
    end_date = which.max(data$detected_at), 
    resolution){
    
    require(dplyr)
    require(lubridate)
    
    
    }   
