#' Generate species code
#'
#' Generates a 6 character species code based on Scientific name of a species.
#'
#' @param x character, scientific name
#' @return A character string with species code.
getSpeciesCode <- function(x){
  s <- stringr::str_split(x, pattern=" ", simplify = T)
  s[] <- stringr::str_sub(s, 1, 3)
  s[] <- stringr::str_to_upper(s)
  code <- paste0(s[,1], s[,2])
  return(code)
}
