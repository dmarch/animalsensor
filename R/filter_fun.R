#' Filter near-duplicate positions
#'
#' @param data data.frame with location data
#' @param step.time time difference to consider duplicated positions, in hours
#' @param step.dist spatial distance to consider duplicated poisitions, in km
#' @return new data.frame with filtered data
filter_dup  <- function (data, step.time = 2/60, step.dist = 0.001){

  ## Keep original number of locations
  loc0 <- nrow(data)

  ## Convert standard format to SDLfilter

  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc[data$lc == "G"] <- 4
  data$lc <- as.numeric(data$lc)

  # Rename columns
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"

  ### Remove duplicated locations, based on both time and space criteria
  data <- SDLfilter::dupfilter(data.frame(data), step.time=step.time, step.dist=step.dist, conditional = FALSE)

  ## Back transform data.frame to standar format

  # Rename columns
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"

  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A"
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == 4] <- "G"

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

  ## Keep original number of locations
  loc0 <- nrow(data)

  ## Convert standard format to SDLfilter

  # Standardize Location clasess
  data$lc <- as.character(data$lc)
  data$lc[data$lc == "A"] <- -1
  data$lc[data$lc == "B"] <- -2
  data$lc[data$lc == "Z"] <- -9
  data$lc[data$lc == "G"] <- 4
  data$lc <- as.numeric(data$lc)

  # Rename columns
  names(data)[names(data)=="date"] <- "DateTime"
  names(data)[names(data)=="lc"] <- "qi"


  ## Filter out values above speed threshold, considering both previous and subsequent positions
  data <- SDLfilter::ddfilter.speed(data, vmax = vmax, method = method)

  ## Back transform data.frame to standar format

  # Rename columns
  names(data)[names(data)=="DateTime"] <- "date"
  names(data)[names(data)=="qi"] <- "lc"

  # Standardize Location clasess
  data$lc[data$lc == -1] <- "A"
  data$lc[data$lc == -2] <- "B"
  data$lc[data$lc == 4] <- "G"

  ## Prepare output
  return(data)
}

