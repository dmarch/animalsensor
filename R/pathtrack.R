#' Proces Pathtrack location data
#'
#' @param txt_file Path to txt file from PathTrack.
#' @param deploymentDateTime POSIXct date time. If NULL, the function will not filter data by date.
#' @return A data.frame with standardized names and parsed times from L0 product.
pathTrack2L0 <- function(txt_file, date_deploy=NULL){

  #extract tag id from filename
  tagid <- stringr::str_extract(txt_file, pattern = '["Tag"]+[[:digit:]]{5}')

  #read TXT file
  data <- data.table::fread(txt_file)

  #apply column names to data.frame
  names(data) <- c("day", "month", "year", "hour", "minute", "second", "secondOfDay",
                   "gpsSatelliteCount", "latitude", "longitude", "altitude",
                   "clockOffset", "residualsGPS", "batteryLevel")

  #pre-process data
  # Accuracy indicator
  # As a general rule, values below 30 indicate good position data,
  # whilst those above 30 have greater errors associated with them
  data <- data %>%
    dplyr::mutate(organismID = tagid,
           time = lubridate::parse_date_time(paste(day, month, year, hour, minute, second), "dmy HMS"))

  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- dplyr::filter(data, time >= date_deploy)

  # Reorder column names
  data <- dplyr::select(data, organismID, time, longitude, latitude, altitude,
                        gpsSatelliteCount, residualsGPS, clockOffset,  batteryLevel)
  return(data)
}

