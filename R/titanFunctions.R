# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @import tidyverse tools janitor rvest
NULL

#' Import and clean the Warehouse transfers report
#'
#' This function imports the badly formatted Titan Warehouse Transfer Report and
#' moves the product and product identifier to the same row as the transfer
#' reports.
#'
#' @param inFile This is the directory of an HTML Warehouse Transfers Report
#' @return A data frame with the product and identifier values appropriately placed
#' @export

cleanWHTransfers <- function(inFile) {
  step1 <- readTitanHTML(inFile) %>%
    mutate(
      internalHeader = ifelse(
        expectedDate == quantity,1,0
      ),
      idGrouping = cumsum(internalHeader)
    )

  idTable <- step1 %>%
    filter(internalHeader == 1) %>%
    select(idGrouping,receivingBuilding) %>%
    rename(headerString = receivingBuilding) %>%
    mutate(
      identifier = str_extract(headerString,'\\(([^)]+)\\)'),
      identifier = removeDiscontinued(identifier),
      identifier = str_trim(gsub('[[:punct:] ]+','',identifier)),

      product = sub('.*?\\)','',headerString),
      product = sub('^\\)','',product),
      product = stringr::str_trim(product),

    ) %>% select(-headerString)

  step1 %>%
    filter(internalHeader == 0) %>%
    left_join(
      idTable, by = 'idGrouping'
    ) %>%
    mutate(
      expectedDate = as.POSIXct(expectedDate, format = '%b %d, %Y', tz = 'UTC'),
      quantity = as.numeric(quantity)
    ) %>%
    select(
      -internalHeader,-idGrouping
    )
}

#' Convert an HTML report to a CSV
#'
#' This function imports a Titan HTML report and converts it to a CSV
#'
#' @param inFile Path to the input file
#' @return a csv file in the same directory as the html file
#' @export

convertTitanHTML <-  function(inFile) {
  outFile <- readTitanHTML(inFile)

  outFileDir <- sprintf(
    '%s.csv',
    tools::file_path_sans_ext(inFile)
  )

  write.csv(
    outFile,
    outFileDir,
    na = '',
    row.names = F
  )
}

#' Clean and convert a Warehouse Transfers report to csv
#'
#' This function imports an HTML Warehouse Transfers Report, converts it to a
#' friendlier format, then exports it as a csv in the same directory.
#'
#' @param inFile Path to the input file
#' @return a csv file in the same directory as the html file
#' @export

convertWHTransfers <- function(inFile) {
  outFile <- cleanWHTransfers(inFile)

  outFileDir <- sprintf(
    '%s.csv',
    tools::file_path_sans_ext(inFile)
  )

  write.csv(
    outFile,
    outFileDir,
    na = '',
    row.names = F
  )
}


#' Import a Titan HTML report
#'
#' This function simplifies the process of importing and cleaning a report from
#' Titan
#'
#' @param inFile Path to the saved Titan HTML report
#' @return A data frame containing the content of the Titan Report
#' @export

readTitanHTML <- function(inFile) {
  inFile %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    .[[1]] %>%
    janitor::clean_names(case='lower_camel')
}


#' Remove DISCONTINUED string from the Identifier column
#'
#' This function addresses the bad data practice of altering product keys in our
#' inventory database by writing DISCONTINUED after them when they cease using
#' them. It's pretty simple, and removes everything after the first space in a
#' string.
#'
#' @param vector A vector to remove the "Discontinued" string from
#' @return A vector with 'Discontinued' removed from all of the strings. More
#'   specifically, a vector with everything after the first space removed.
#' @export

removeDiscontinued <- function(vector) {
  returnVector <- sub('\\s(.+)','',vector)
  return(returnVector)
}

#' Separate and create multiple line items in the Purchasing report in instances
#' where a report combines line items in this convention - 'x unit & y case'
#'
#' Titan designs some of its reports, most notably the purchasing report, to
#' list units and cases in the same line item. In order to get around this
#' convetion, this command will split those items and produce separate line
#' items, one for ases, and one for units.
#'
#' @param df A Titan report containing lines with units and cases listed
#'   together in one line item.
#' @param var The variable in which cases and units are listed together
#'   formatted 'x units & y cases'.
#' @return a new dataset where the units and cases are split into multiple line
#'   items.
#' @export

sepMultipleInventory <- function(df, var) {

  varName <- deparse(substitute(var))

  df %>%
    mutate(totSplits = stringr::str_count({{ var }},'&')) %>%
    separate({{ var }}, into = paste0('var',seq(1:max(.$totSplits)+1)),sep= ' & ') %>%
    pivot_longer(
      cols = paste0('var',seq(1:max(.$totSplits)+1)),
      values_to = varName,
      values_drop_na = T
    ) %>%
    select(-totSplits,-name)

}

#' Split a text units column into the number and the unit.
#'
#' Titan reports build their units columns as strings, formatted as such 'n
#' units'. This function splits the number from the units, and converts the
#' number to a numeric value.
#'
#' @param df A Titan report containing a string units column formatted 'n
#'   units'.
#' @param var The Variable containing a string units column formatted 'n units'.
#' @return a data frame with a string units column and a numeric total units
#'   column.
#' @export

sepQuantAndUnit <- function(df, var) {

  df %>%
    separate(
      {{ var }}, into = c('quantity','unitOfMeasure'),
      extra = 'merge',
      sep = ' '
    ) %>%
    mutate(quantity = as.numeric(quantity))

}
















