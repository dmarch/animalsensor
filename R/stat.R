#' Proces STAT location data
#'
#' @param stat_file Path to csv file from STAT.
#' @param date_deploy POSIXct date time. If NULL, the function will not filter data by date.
#' @return A data.frame with standardized names and parsed times from L0 product.
stat2L0 <- function(stat_file, date_deploy=NULL){

  #read CSV file
  #standard import (read.csv) does not work with headerless columns
  #so reading whole table then extract useful info
  data = read.table(stat_file, header=F, fill = TRUE, sep = ",", stringsAsFactors=F)

  #extract column names from first row
  Header_Names = data[1,]

  #apply column names to dataframe
  names(data) = Header_Names

  #remove first row (duplicate header row)
  data = data[-1,]

  #removes any rows that may have been errononeously added
  #by the read table function
  #remove rows where Tag ID value is not the proper value
  #as reported by first ID cell
  data = data[data$tag_id == data$tag_id[1],]

  ### Rename columns
  names(data)[names(data)=="tag_id"] <- "id"
  names(data)[names(data)=="utc"] <- "date"
  names(data)[names(data)=="lon1"] <- "lon"
  names(data)[names(data)=="lat1"] <- "lat"
  names(data)[names(data)=="lc"] <- "lc"

  # Reorder column names
  data <- data[, !duplicated(colnames(data))]
  data <- dplyr::select(data, id, date, lon, lat, lc)

  # Convert character values to numeric and POSIXct
  data <- data %>% dplyr::mutate_each(funs(as.numeric), lat, lon) %>%
    dplyr::mutate_each(funs(lubridate::parse_date_time(.,"Ymd HMS")), date)

  # Select data collected from the date of deployment
  if (!is.null(date_deploy)) data <- dplyr::filter(data, date >= date_deploy)

  # Reorder column names
  data <- dplyr::select(data, id, date, lon, lat, lc)
  return(data)
}
