#read_in.py
#reads data in for use by other functions


#read in all tags
get_all_tags <- function(tags_text_file){
    alltags <- read.csv(tags_text_file,
                        skip = 1,
                        blank.lines.skip = TRUE,
                        colClasses = c("NULL", "character"),
                        header = FALSE,
                        col.names = c("","tag")
                        )
    return(alltags)
}

tags_with_species_code <- function(tags_text_file){
    alltags <- read.csv(tags_text_file,
                        skip = 1,
                        blank.lines.skip = TRUE,
                        colClasses = c("NULL", "character", "character"),
                        header = FALSE,
                        col.names = c("","tag", "species")
    )
    return(alltags)
}

#'Create class to hold datetime
#' from character to new class
setClass('myDate')
setAs("character", "myDate", function(from) as.POSIXct(from, format = "%m-%d-%Y %H:%M:%S"))


#' Get data to work with
#' @param detection_data The containing pittag detections as directly downloaded
get_detection_data <- function(detection_data){
    alldata <- read.csv(detection_data,
                        header = TRUE,
                        colClasses = c("character", "myDate", "factor", "factor"))
    #add date time columns
    alldata <- add_datetime_columns(alldata)
    return(alldata)
}

#' add_datetime_columns
#' Adds columns that allow for easier temporal deliniation
#' add date columns with lubridate for use in querying by date
add_datetime_columns <- function(detection_dataframe){
    require(lubridate)
    detection_dataframe$year  <- year(detection_dataframe$detected_at)
    detection_dataframe$month <- month(detection_dataframe$detected_at)
    detection_dataframe$week  <- week(detection_dataframe$detected_at)
    detection_dataframe$day   <- day(detection_dataframe$detected_at)
    detection_dataframe$hour  <- hour(detection_dataframe$detected_at)
    return(detection_dataframe)
}
