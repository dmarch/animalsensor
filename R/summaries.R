#' Summarize trip segment data
#'
#' @param data Data.frame.
#' @param id column name of the animal id.
#' @param date column name of the date, with POSIXct format.
#' @param lon column name of longitude.
#' @param lat column name of latitude.
#' @return A data.frame with summary data per trip.
summarizeTrips <- function(data, id = "organismID", trip = "tripID", date ="time", lon = "longitude", lat = "latitude"){

  df <- data %>%
    # rename varaibles
    dplyr::rename(id = `id`, trip = `trip`, date = `date`, lon = `lon`, lat = `lat`) %>%
    # order by date
    dplyr::arrange(date) %>%
    # group by id and trip
    dplyr::group_by(id, trip) %>%
    dplyr::summarize(date_deploy = first(date),
                     lon_deploy = first(lon),
                     lat_deploy = first(lat),
                     date_last = last(date),
                     time_interval_h = median(as.numeric(difftime(tail(date, -1), head(date, -1), units="hours"))),
                     distance_km = sum(geosphere::distGeo(p1 = cbind(lon, lat)), na.rm=TRUE)/1000,  # segment distance
                     n_loc = n()) %>%  # get first and last observations
    dplyr::mutate(duration_h = round(difftime(date_last, date_deploy, units="hours"))) %>%  # calculate duration of the track
    # rename to original variables
    dplyr::rename(`id` = id)

  return(df)
}
