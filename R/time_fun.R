#' Segment trips based on temporal threshold
#'
#' @param x POSIXct date time
#' @param thrs time threshold criteria to split into segments, in hours
#' @return A vector with segments IDs
timedif.segment <- function(x, thrs){

  # order time series
  x <- x[order(x)]

  # calculate difference between time steps
  tdif <- c(NA,as.numeric(difftime(x[-1], x[-length(x)], units="hours" )))

  # get breakpoints based on time threshold
  breaks <- c(0, which(tdif >= thrs), length(tdif)+1)

  # split into segments
  intervals <- cut(order(x), breaks, right=FALSE)
  segments <- as.numeric(intervals)
  return(segments)
}
