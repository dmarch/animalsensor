#' Filter near-duplicate positions
#'
#' @param data data.frame with location data
#' @param step.time time difference to consider duplicated positions, in hours
#' @param step.dist spatial distance to consider duplicated poisitions, in km
#' @return new data.frame with filtered data
filter_dup  <- function (data, step.time = 2/60, step.dist = 0.001){

  ## Convert standard format to SDLfilter

  # Standardize Location clasess
  data$argosLC <- as.character(data$argosLC)
  data$argosLC[data$argosLC == "A"] <- -1
  data$argosLC[data$argosLC == "B"] <- -2
  data$argosLC[data$argosLC == "Z"] <- -9
  data$argosLC[data$argosLC == "G"] <- 4
  data$argosLC <- as.numeric(data$argosLC)

  # Rename columns
  names(data)[names(data)=="time"] <- "DateTime"
  names(data)[names(data)=="argosLC"] <- "qi"
  names(data)[names(data)=="longitude"] <- "lon"
  names(data)[names(data)=="latitude"] <- "lat"
  names(data)[names(data)=="organismID"] <- "id"

  ### Remove duplicated locations, based on both time and space criteria
  data <- SDLfilter::dupfilter(data.frame(data), step.time=step.time, step.dist=step.dist, conditional = FALSE)

  ## Back transform data.frame to standard format

  # Rename columns
  names(data)[names(data)=="DateTime"] <- "time"
  names(data)[names(data)=="qi"] <- "argosLC"
  names(data)[names(data)=="lon"] <- "longitude"
  names(data)[names(data)=="lat"] <- "latitude"
  names(data)[names(data)=="id"] <- "organismID"

  # Standardize Location clasess
  data$argosLC[data$argosLC == -1] <- "A"
  data$argosLC[data$argosLC == -2] <- "B"
  data$argosLC[data$argosLC == 4] <- "G"

  ## Prepare output
  return(data)
}


#' Speed filter
#'
#' @param data data.frame with location data
#' @param vmax value of the maximum of velocity using, in km/h
#' @param method see ?ddfilter.speed
#' @return new data.frame with filtered data
filter_speed  <- function (data, vmax = 10.8, method = 1){

  ## Convert standard format to SDLfilter

  # Standardize Location clasess
  data$argosLC <- as.character(data$argosLC)
  data$argosLC[data$argosLC == "A"] <- -1
  data$argosLC[data$argosLC == "B"] <- -2
  data$argosLC[data$argosLC == "Z"] <- -9
  data$argosLC[data$argosLC == "G"] <- 4
  data$argosLC <- as.numeric(data$argosLC)

  # Rename columns
  names(data)[names(data)=="time"] <- "DateTime"
  names(data)[names(data)=="argosLC"] <- "qi"
  names(data)[names(data)=="longitude"] <- "lon"
  names(data)[names(data)=="latitude"] <- "lat"
  names(data)[names(data)=="organismID"] <- "id"


  ## Filter out values above speed threshold, considering both previous and subsequent positions
  data <- SDLfilter::ddfilter_speed(data, vmax = vmax, method = method)

  ## Back transform data.frame to standard format

  # Rename columns
  names(data)[names(data)=="DateTime"] <- "time"
  names(data)[names(data)=="qi"] <- "argosLC"
  names(data)[names(data)=="lon"] <- "longitude"
  names(data)[names(data)=="lat"] <- "latitude"
  names(data)[names(data)=="id"] <- "organismID"

  # Standardize Location clasess
  data$argosLC[data$argosLC == -1] <- "A"
  data$argosLC[data$argosLC == -2] <- "B"
  data$argosLC[data$argosLC == 4] <- "G"

  ## Prepare output
  return(data)
}

