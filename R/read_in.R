#' Read in tags
#'
#' Reads in tags from .csv file. Expects row numbers in first column that are
#' removed. Skips first line as column name.
#' @param tags_text_file .csv file containing tags in the the second column.
#' @return Returns dataframe with one column named "tags" for use with other
#'   functions in package.
#' @export

#read in all tags
get_tag_list <- function(tags_text_file){
    alltags <- read.csv(tags_text_file,
                        skip = 1,
                        blank.lines.skip = TRUE,
                        colClasses = c("NULL", "character"),
                        header = FALSE,
                        col.names = c("","tag"),
                        stringsAsFactors = FALSE
                        )
    return(alltags)
}

#' tags_with_species_code
#'
#' Gets tag numbers and species code from csv file
#' @param tags_text_file A csv file containing tags and species code. Expects
#'   file to how column of row numbers but then removes them. Skips line
#'   containing columnames.
#' @return Returns dataframe with two columns "tag" and "species" to be used
#'   with other functions in this package.
#' @export

tags_with_species_code <- function(tags_text_file){
    alltags <- read.csv(tags_text_file,
                        skip = 1,
                        blank.lines.skip = TRUE,
                        colClasses = c("NULL", "character", "character"),
                        header = FALSE,
                        col.names = c("","tag", "species"),
                        stringsAsFactors = FALSE
    )
    return(alltags)
}



#' Class to import datetime from character variable in original dataset in correct format.

setClass('myDate')
setAs("character", "myDate", function(from) as.POSIXct(from, format = "%m-%d-%Y %H:%M:%S"))




#' get_detection_data
#'
#' Reads in detection data for further analysis using the tools in this package.
#' You can read in multiple data sets and then rbind them together to perform an
#' analysis on all data.
#'
#'
#' @param data_set Path to data containing pittag detections as directly
#'   downloaded from Biomark's BIOlogic in .csv format. Use forward slashes to
#'   specify path. Field names should be as follows: "tag", "detected_at",
#'   "reader", "antenna". Data from other sources may be used if the follow this
#'   same format, with the 'detected_at' field containing data in a 'mm-dd-yyyy
#'   hh:mm:ss' format.
#' @return Returns dataframe with variables: tag, detected_at, reader_antenna,
#'   year, month, week, day, hour.
#' @export


get_detection_data <- function(data_set){
    alldata <- read.csv(data_set,
                        header = TRUE,
                        colClasses = c("character", "myDate", "factor",  "factor"))

    #add date time columns
    alldata <- add_datetime_columns(alldata)
    return(alldata)
}






#' add_datetime_columns Adds columns that allow for easier temporal deliniation
#' @param detection_dataframe Data read in using
#'   \code{\link{get_detection_data}} that contains a the column 'detected_at'
#'   with date/time date in it.
#' @return Original dataframe with additional columns containing integer values
#'   for year, month, week, day, and hour.

add_datetime_columns <- function(detection_dataframe){
    detection_dataframe$year  <- lubridate::year(detection_dataframe$detected_at)
    detection_dataframe$month <- lubridate::month(detection_dataframe$detected_at)
    detection_dataframe$week  <- lubridate::week(detection_dataframe$detected_at)
    detection_dataframe$day   <- lubridate::day(detection_dataframe$detected_at)
    detection_dataframe$hour  <- lubridate::hour(detection_dataframe$detected_at)
    return(detection_dataframe)
}
