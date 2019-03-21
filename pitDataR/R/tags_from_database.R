#' tags_from_db
#'
#'
#' function to extract tags along with their corresponding species from a
#' database following the GCMRC convention
#'
#' @param database_path The path to the database you want to use, use forward
#'   slashes to seperate directories. Follows GCMRCC database format and
#'   extracts the 'PITTAG' and 'SPECIES_CODE' from the 'FISH_T_SPECIMEN'
#'   table in the database.
#' @return Returns data frame of two variables. 'FISH_T_SPECIMEN.PITTAG' is
#'   returned as "tag" in the new dataframe and 'FISH_T_SPECIMEN.SPECIES_CODE'
#'   is returned as "species" so that the variable names match those found
#'   throughout the 'pitDataR' package.
#' @section Warning: Must use 32-bit R version to function.
#' @export


tags_from_db <- function(database_path){

    #you have to import RODBC and switch to 32 bit mode
    if(!requireNamespace("RODBC", quietly = TRUE)){
        stop("RODBC needs to be installed and your R version set to 32 bit.
             Manually install RODBC then use 'Tools > Global Options > R                     version > Change' to set version to 32-bit.",
             call. = FALSE)
    }

    #connect to database with RODBC
    channel <- RODBC::odbcConnectAccess(database_path)

    #query to get data
    tag_df <- RODBC::sqlQuery(channel, paste(
        "SELECT DISTINCT(FISH_T_SPECIMEN.PITTAG), FISH_T_SPECIMEN.SPECIES_CODE
        FROM FISH_T_SPECIMEN
        WHERE FISH_T_SPECIMEN.PITTAG IS NOT NULL;"
        )
    )

    RODBC::odbcClose(channel = channel)

    #rename columns
    colnames(tag_df) <- c("tag", "species")
    tag_df$tag <- as.character(tag_df$tag)
    tag_df$species <- as.character(tag_df$species)

    return(tag_df)

}
