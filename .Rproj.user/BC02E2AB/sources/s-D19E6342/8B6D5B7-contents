#read_in.py
#reads data in for use by other functions


#read in all tags
get_all_tags <- function(tags_text_file){
    alltags <- read.csv(tags_text_file,
                        skip = 1,
                        blank.lines.skip = TRUE,
                        colClasses = c("NULL", "factor"),
                        header = FALSE,
                        col.names = c("","tag")
                        )
    return(alltags)
}

#function to get date in correct format
#format can be changed to get correct
# setClass('myDate')
# setAs("character", "myDate", function(from) as.POSIXct(from, format = "%m-%d-%Y %H:%M:%S"))
#

#read in detection data
get_detection_data <- function(detection_data){
    setClass('myDate')
    setAs("character", "myDate", function(from) as.POSIXct(from, format = "%m-%d-%Y %H:%M:%S"))
    alldata <- read.csv(detection_data,
                        header = TRUE,
                        colClasses = c("character", "myDate", "factor", "factor"))
    #add date time columns
    alldata <- add_datetime_columns(alldata)
    return(alldata)
}

#add date columns with lubridate for use in querying by date
add_datetime_columns <- function(detection_dataframe){
    require(lubridate)
    detection_dataframe$year  <- year(detection_dataframe$detected_at)
    detection_dataframe$month <- month(detection_dataframe$detected_at)
    detection_dataframe$week  <- week(detection_dataframe$detected_at)
    detection_dataframe$day   <- day(detection_dataframe$detected_at)
    detection_dataframe$hour  <- hour(detection_dataframe$detected_at)
    return(detection_dataframe)
}
