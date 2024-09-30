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

#' Import the full coordinator data set from Github
#'
#' This function provides quick access to the Coordinator List, which is
#' valuable for many tasks in Food & Nutrition Services.
#'
#' @return the full, historical record of all coordinators at schools
#' @export

fnsCoordList = function() {

  githubDir <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/coordinatorList.csv'

  returnDF <- readr::read_csv(githubDir, col_types = c('ccccc')) %>%
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
    dplyr::ungroup()

  return(df)

}
