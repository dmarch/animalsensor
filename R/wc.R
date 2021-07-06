#' Process Wildlife Computers location data
#'
#' @param data Data.frame.
#' @param locale System time locale.
#' @param deploymentDateTime POSIXct date time. If NULL, the function will not filter data by date.
#' @return A data.frame with standardized names and parsed times from L0 product.
wcLoc2L0 <- function(data, locale = "English", date_deploy=NULL){

  ### Rename columns
  names(data)[names(data)=="Ptt"] <- "id"
  names(data)[names(data)=="Date"] <- "date"
  names(data)[names(data)=="Longitude"] <- "lon"
  names(data)[names(data)=="Latitude"] <- "lat"
  names(data)[names(data)=="Quality"] <- "lc"
  names(data)[names(data)=="Error.Semi.major.axis"] <- "smaj"
  names(data)[names(data)=="Error.Semi.minor.axis"] <- "smin"
  names(data)[names(data)=="Error.Ellipse.orientation"] <- "eor"

  # Convert to POSIXct
  data$time <- lubridate::parse_date_time(data$date, c("HMS dbY", "Ymd HMS"), locale=locale, tz="UTC")

  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- dplyr::filter(data, time >= date_deploy)

  # Reorder column names
  data <- dplyr::select(data, id, time, lon, lat, lc, smaj, smin, eor)
  return(data)
}

