#' tags_from_database.r
#' function to extract tags from databas
#' @param database_path The path to the database youwant to use, use forward slashes
#'
#' Function to extract list of tags from database, either for a particular
#' species or for all species
#' must be running 32 bit R for this to function

get_tags_from_db_all_species <- function(database_path){

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
