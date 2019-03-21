#'plot_number_species_by_resolution.r
#'
#'
#'
#'

plot_by_species_resolution <-function(data_set, start_date = NULL, end_date = NULL, tags, detection_resolution){
    #get detected
    detections <- detected_in_period(data_set = dataset,
                                     start_date = start_date,
                                     end_date = end_date,
                                     detection_resolution = detection_resolution)

    detections_with_species <- left_join(detections, tags)

    #aggregate data by date and species
    detections_with_species <- detections_with_species %>%
        group_by(species)

    species_detection_plot <- ggplot2::ggplot(detections_with_species, aes)
}
