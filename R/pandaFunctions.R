#' @importFrom stringr str_pad str_sub
NULL

#' Convert a numeric grade vector to a character grade vector
#'
#' This function takes a numeric grade vector or column and converts it to a
#' character. For example, -2 is converted to 'K0', 0 is converted to 'K2', 9 to
#' '09', and 11 to '11'
#'
#' @param vector a numeric grade vector (kindergarten expressed as 0, -1, -2)
#' @return a character grade vector (K0,K1,K2,01,02)
#' @export

kinderNumToStr <- function(vector) {
  strVector <- ifelse(
    vector < 1,
    sprintf('K%s',vector+2),
    stringr::str_pad(vector,2,pad = '0')
  )

  return(strVector)
}

#' Convert a character grade vector to a numeric grade vector
#'
#' This function takes a character grade vector or column and converts it to a
#' number For example, 'K0' is converted to -2, 'K2' is converted to 0, '09' to
#' 9, and '11' to 11
#'
#' @param vector a character grade vector ('K0','K1','K2','01','02')
#' @return a numeric grade vector (-2,-1,0,1,2)
#' @export

kinderStrToNum <- function(vector) {
  numVector <- ifelse(
    grepl('K',vector),
    as.numeric(stringr::str_sub(vector,2,2))-2,
    as.numeric(vector)
  )

  return(numVector)
}

