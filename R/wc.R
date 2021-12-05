#' Process Wildlife Computers location data
#'
#' @param data Data.frame.
#' @param locale System time locale.
#' @param deploymentDateTime POSIXct date time. If NULL, the function will not filter data by date.
#' @return A data.frame with standardized names and parsed times from L0 product.
wcLoc2L0 <- function(data, locale = "English", date_deploy=NULL){

  require(dplyr)

  # prepare data
  data <- data %>%
    # rename variables
    dplyr::rename(organismID = Ptt,
                  longitude = Longitude,
                  latitude = Latitude,
                  argosLC = Quality,
                  time = Date,
                  argosSemiMajor = Error.Semi.major.axis,
                  argosSemiMinor = Error.Semi.minor.axis,
                  argosOrientation = Error.Ellipse.orientation
    ) %>%
    # parse date time
    dplyr::mutate(time = lubridate::parse_date_time(time, c("HMS dbY", "Ymd HMS"), locale=locale, tz = "UTC")) %>%
    # filter data without coordinates
    dplyr::filter(!is.na(longitude)) %>%
    # select variables
    dplyr::select(organismID, time, longitude, latitude, argosLC, argosSemiMajor, argosSemiMinor, argosOrientation)

  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- dplyr::filter(data, time >= date_deploy)

  # return data
  return(data)
}
