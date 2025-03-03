# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @importFrom dplyr anti_join filter group_by mutate n row_number select
#'   summarise ungroup
#' @importFrom magrittr %>%
#' @importFrom readr read_csv
#' @importFrom stringr str_pad

NULL

#' Clean up strings with $ and , values embedded in them and convert them to
#' numeric values.
#'
#' @param vector a string vector containing non-numeric character vectors
#'   intended as cash values
#' @return a numeric vector
#' @export

cashStringCleaner = function(vector) {
  newVector <- as.numeric(gsub('[^0-9.-]','',vector))
  return(newVector)
}

#' Imports the average labor hours by category for our staff
#'
#' @return a dataset containing average labor hours per year & the % multiplier
#'   for calculating benefits
#' @export

fnsAvgLaborCost = function() {

  githubDir <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/coordinatorList.csv'

  returnDF <- readr::read_csv(githubDir, col_types = 'ncnn')

  return(returnDF)

}

#' Import the full coordinator data set from Github
#'
#' This function provides quick access to the Coordinator List, which is
#' valuable for many tasks in Food & Nutrition Services.
#'
#' @return the full, historical record of all coordinators at schools
#' @export

fnsCoordList = function() {

  githubDir <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/coordinatorList.csv'

  returnDF <- readr::read_csv(githubDir, col_types = c('cccccc')) %>%
    dplyr::mutate(
      date = as.POSIXct(date,format = '%m/%d/%Y', tz = 'UTC'),
      titanID = stringr::str_pad(titanID,3,pad='0')
      )

  return(returnDF)

}

#' Import the current coordinator data set from Github
#'
#' This function provides quick access to the Coordinator List, which is
#' valuable for many tasks in Food & Nutrition Services.
#'
#' @return the currently assigned coordinators at schools
#' @export

fnsCoordListByDate = function(cutOff=Sys.Date()) {

  df <- fnsCoordList() %>%
    dplyr::filter(date < cutOff) %>%
    dplyr::group_by(deseID,titanID) %>%
    dplyr::filter(date == max(date)) %>%
    dplyr::filter(fc != 'DISCONTINUED') %>%
    dplyr::ungroup()

  return(df)

}
