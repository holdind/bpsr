# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @import lubridate stringr
NULL

#' Extract the current fiscal year from a date value
#'
#' Since School years don't perfectly match to calendar years, this function is
#' designed to calculate the current school fiscal year from a date. The fiscal
#' year is a numeric value, meaning it's more useful than the SYXXXX school year
#' format.
#'
#' @param date This is a vector containing date information
#' @return the current school fiscal year for that date in numeric format
#' @export

fiscYearFromDate <- function(date) {

  fiscYear <- lubridate::year({{ date }}) + ifelse(
    lubridate::month({{ date }}) > 8,
    1,0
  )

  return(fiscYear)

}

#' Convert a string school year value into a numeric fiscal year value
#'
#' Since the string format for dates is frankly bad for working with time, this
#' turns the school year value "SY0000" format into a numeric value equal to the
#' second digits + 2000. Which means that if, somehow, this package is still
#' being deployed in 2100, you'll need to make this more dynamic somehow.
#'
#' @param schYear a string school year value formatted "SY0000"
#' @return A vector containing the numeric Fiscal year value, which is equal to
#'   the second part of the school year + 2000
#' @export

schToFiscalYear <- function(schYear) {

  schYear <- as.numeric(stringr::str_sub({{ schYear }}, 5,6)) + 2000

  return(schYear)

}

#' Extract the current school year from a date value
#'
#' Since School years don't perfectly match to calendar years, this function is
#' designed to calculate the current school year from a date. School Year is a
#' string value, formatted 'SY0000', where the first set of zeroes indicate the
#' year when a school year starts, and the second set indicates the year a
#' school year ends.
#'
#' @param date This is a vector containing date information
#' @return the current school year for that date in string format
#' @export

schYearFromDate <- function(date) {

  fiscTempura <- fiscYearFromDate(date)

  schYear <- sprintf(
    'SY%s%s',
    stringr::str_pad(fiscTempura%%100-1,2,pad='0'),
    stringr::str_pad(fiscTempura%%100,2,pad='0')
  )

  return(schYear)

}
