# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @import lubridate stringr RCurl tidyverse
NULL


#' Import the days off calendar from Github
#'
#' Having quick access to a school year calendar is extremely valuable. To do
#' that, We need to have access to the days off so that we can remove them from
#' the calendar.
#'
#' @return the days off data set, which includes calendar days off and an
#'   explanation for the event
#' @export

bpsSchedDaysOff <- function() {

  githubDir <- RCurl::getURL('https://raw.githubusercontent.com/holdind/bpsr/main/data/bpsHolidays.csv')

  daysOff <- read.csv(text = githubDir) %>%
    dplyr::mutate(date = as.POSIXct(date,format = '%m/%d/%Y', tz = 'UTC'))

  return(daysOff)

}

#' Import a data set containing all scheduled school days in Boston Public
#' Schools for the upcoming school year
#'
#' This command creates a dataframe containing a list of all scheduled school
#' dates. It provides an option for a selective date range.
#'
#' @return a dataframe containing dates for the selected date range, including
#'   fiscYear, date, and day of the week.
#' @param startYear a numeric value representing the first fiscal year to pull.
#'   Note that a fiscal year is the second year in a school year, so the fiscal
#'   year for 2022-23 would be 2023.
#' @param endYear a numeric value representing the last fiscal year to pull.
#'   Note that a fiscal year is the second year in a school year, so the fiscal
#'   year for 2022-23 would be 2023.
#' @export

bpsScheduleDates <- function(startYear=NA,endYear=NA) {

  githubDir <- RCurl::getURL('https://raw.githubusercontent.com/holdind/bpsr/main/data/bpsCalendarLengths.csv')
  posixDayLength = 60*60*24

  # drop days off
  daysOff <- bpsSchedDaysOff()

  # drop weekends
  weekend <- c('Saturday','Sunday')

  baseDF <- read.csv(text = githubDir)

  if(!is.na(startYear)) {
    if(!is.na(endYear)) {
      baseDF <- baseDF %>%
        dplyr::filter(fiscYear>=startYear & fiscYear<=endYear)
    } else {
      baseDF <- baseDF %>%
        dplyr::filter(fiscYear>=startYear)
    }
  } else if(!is.na(endYear)) {
    baseDF <- baseDF %>%
      dplyr::filter(fiscYear<=endYear)
  }

  baseDFLen <- nrow(baseDF)
  if(baseDFLen < 1) {
    stop('selected school years unavailable')
  }

  df <- baseDF %>%
    dplyr::mutate(
      start = as.POSIXct(start,format = '%m/%d/%Y', tz = 'UTC'),
      end = as.POSIXct(end,format = '%m/%d/%Y', tz = 'UTC'),
      range = as.numeric(end-start+1)
    ) %>%
    tidyr::uncount(range) %>%
    dplyr::mutate(
      date = start+dplyr::row_number()*posixDayLength-posixDayLength,
      dow = weekdays(date)
    ) %>%
    dplyr::select(fiscYear,date,dow) %>%
    dplyr::filter(!(dow %in% weekend)) %>%
    dplyr::anti_join(daysOff, by = 'date')

  return(df)

}

#' return a dataframe containing the number of school days in each month in BPS
#'
#' This command returns a data frame containing the fiscal year, month, and
#' total school days each month in BPS
#'
#' @return a dataframe containing fiscal year, month, and the total school days
#'   in the month
#' @param startYear a numeric value representing the first fiscal year to pull.
#'   Note that a fiscal year is the second year in a school year, so the fiscal
#'   year for 2022-23 would be 2023.
#' @param endYear a numeric value representing the last fiscal year to pull.
#'   Note that a fiscal year is the second year in a school year, so the fiscal
#'   year for 2022-23 would be 2023.
#' @export

bpsMonthDateCount <- function(startYear=NA,endYear=NA) {

  scheduleData <- bpsScheduleDates(startYear=startYear,endYear=endYear)

  df = scheduleData %>%
    dplyr::mutate(month = yyyymmString(date)) %>%
    dplyr::group_by(fiscYear,month) %>%
    dplyr::summarise(totalDays = n())

  return(df)

}

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

#' Build a date string formated YYYYMMDD
#'
#' This function returns a date string formatted YYYYMMDD for use in file
#' directories & file names.
#'
#' @param date the day you want to convert to the string
#' @return a string with the current date formatted YYYYMMDD
#' @export

yyyymmddString <- function(date) {

  s <- sprintf(
    '%s%s',
    stringr::str_remove(yyyymmString(date),'\\-'),
    stringr::str_pad(lubridate::day(date),2,pad = '0')
  )

  return(s)

}

#' Build a current date string formated YYYY-MM
#'
#' This function returns a date string formatted YYYY-MM for use in file
#' directories & file names.
#'
#' @param date the day you want to convert to the string
#' @return a string with the date formatted YYYY-MM
#' @export

yyyymmString <- function(date) {

  s <- sprintf(
    '%s-%s',
    lubridate::year(date),
    stringr::str_pad(lubridate::month(date),2,pad = '0')
  )

  return(s)

}










