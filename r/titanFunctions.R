# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @import tidyverse tools janitor rvest
NULL

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

  returnDF <- df %>%
    dplyr::mutate(totSplits = stringr::str_count({{ var }},'&')) %>%
    separate({{ var }}, into = paste0('var',seq(1:max(.$totSplits)+1)),sep= ' & ') %>%
    pivot_longer(
      cols = paste0('var',seq(1:max(.$totSplits)+1)),
      values_to = varName,
      values_drop_na = T
    ) %>%
    select(-totSplits,-name)

  return(returnDF)

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
#' @param nameTotal What you want to name the new total items variable.
#' @param nameUnits What you want to name the new units variable
#' @return a data frame with a string units column and a numeric total units
#'   column.
#' @export

sepQuantAndUnit <- function(df, var, nameTotal, nameUnits) {

  strTotal <- deparse(substitute(nameTotal))
  strUnits <- deparse(substitute(nameUnits))

  returnDF <- df %>%
    separate(
      {{ var }}, into = c(strTotal,strUnits),
      extra = 'merge',
      sep = ' '
    ) %>%
    mutate({{ nameTotal }} := as.numeric({{ nameTotal }}))

  return(returnDF)

}

#' Convert a Titan building name into a school name and its constituent ID
#'
#' We updated titan building names to contain IDs behind a # mark. This code
#' splits the Titan IDs into a building name and a school code
#' 
#' @param df A Titan report containing a building variable
#' @param var The variable containing the Titan school names
#' @return a data frame with the school ID separated from the school name
#' @export

splitNameAndID <- function(df,var) {
  
  returnDF <- df %>% 
    tidyr::separate({{ var }}, into = c('building','buildingID'), sep = '#') %>% 
    dplyr::mutate(building = stringr::str_trim(building))
  
  return(returnDF)
  
}

#' Convert a daily cash reconcilation report to a csv in the same directory
#'
#' This function imports an HTML cash reconciliation report and converts it to a
#' csv
#'
#' @param inFile the path to the input file
#' @return creates a csv file in the same directory as the html file
#' @export

titanConvertCashRec <- function(inFile) {

  outFile <- titanImportCashRec(inFile)

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

#' Convert an HTML Edit Check Report to a CSV in the same directory
#'
#' This function imports a Titan HTML Edit Check report and converts it to a csv
#'
#' @param inFile Path to the input file
#' @return a csv file in the same directory as the html file
#' @export

titanConvertEditCheck <- function(inFile) {

  outFile <- titanImportEditCheck(inFile)

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

#' Convert an HTML report to a CSV
#'
#' This function imports a Titan HTML report and converts it to a CSV
#'
#' @param inFile Path to the input file
#' @return a csv file in the same directory as the html file
#' @export

titanConvertHTML <-  function(inFile) {
  outFile <- titanImportHTML(inFile)

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

#' Convert a production record file to a csv
#'
#' This function imports a Titan Production Records report & converts it to a
#' CSV
#'
#' @param inFile Path to the html production records report
#' @return a csv file in the same directory as the html file
#' @export

titanConvertProdRecs <- function(inFile,htmlListVars=c('school','date')) {

  outFile <- titanImportProdRecs(inFile)

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

titanConvertWHTransfers <- function(inFile) {
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

#' Convert an HTML daily cash reconciliation report from Titan into an R Data
#' frame
#'
#' This function imports an HTML daily cash reconciliation report in HTML and
#' converts it to a format for use in R
#'
#' @param inFile Path to the input file
#' @return an R dataframe containing the data from the HTML cash reconciliation
#'   report
#' @export

titanImportCashRec <- function(inFile) {
  # Import cash reconciliation data - eventually, we'll add this to BPSR

  titanColNames <- c(
    'date', 'prepaidAccountChanges', 'onlinePayments', 'nonCashAdjustments',
    'cashExpected', 'cashRefund', 'cashTotal', 'cashReceived', 'checksReceived',
    'cashAndChecksTotal', 'overShort'
  )

  html <- rvest::read_html(inFile)

  # This takes all of the html tables and cobbles them together into a single data
  # frame

  tables <- html %>%
    rvest::html_nodes('table') %>%
    map_df(~rvest::html_table(.x,header=F)) %>%
    set_names(titanColNames)

  # This prepares the data set by flagging rows with the school name in them, then
  # creates a running matchID column that will allow us to match school name in
  # later. It also drops data from a summary table at the end of the HTML report.

  df1 <- tables %>%
    dplyr::mutate(
      headerCol = ifelse(
        row_number() == 1 | lag(date) == 'Total',1,0
      ),
      matchID = cumsum(headerCol)
    ) %>%
    filter(!is.na(cashExpected))

  # school names are built into the first of 2 header rows. This table extract
  # them to merge onto the rows below.

  schNameMatch <- df1 %>%
    dplyr::filter(headerCol==1) %>%
    dplyr::select(date,matchID) %>%
    separate(date, sep = ':', into = c('buildingID','building')) %>%
    dplyr::mutate(
      building = stringr::str_trim(building)
    )

  # This final block merges in school names, moves them to the front of the data
  # frame, drops variables created for this process, drops header column rows, and
  # then, finally, converts our string dollar values into numeric & date into
  # POSIXct

  dfFin <- df1 %>%
    left_join(schNameMatch, by = 'matchID') %>%
    relocate(buildingID, building) %>%
    dplyr::filter(headerCol == 0 & date!='Date' & date != 'Total') %>%
    dplyr::select(-c(headerCol,matchID)) %>%
    dplyr::mutate(
      date = as.POSIXct(date, format = "%m/%d/%Y", tz = 'UTC'),
      across(prepaidAccountChanges:overShort, ~ as.numeric(gsub('[\\$,]','',.)))
    ) %>% 
    splitNameAndID('building')

  return(dfFin)

}

#' Import a Titan Edit Check Report
#'
#' This function imports and transforms an HTML Edit Check report so that users
#' can interact with it like a normal data set.
#'
#' @param inFile Path to the Edit Check file
#' @return a cleaned edit check report dataframe
#' @export

titanImportEditCheck <- function(inFile) {

  colNames <- c(
    'date', 'assistanceProgram', 'totalEligible', 'totEligibleTimesAF', 'totalClaims'
  )

  html <- rvest::read_html(inFile)

  # Extract data in tables, create match values by flagging header lines and
  # running a cumsum over them
  tables <- html %>%
    rvest::html_nodes("table") %>%
    map_df(~rvest::html_table(.x, header = FALSE)) %>%
    set_names(colNames) %>%
    dplyr::mutate(
      headerRow = ifelse(assistanceProgram == 'Assistance Program',1,0),
      matchID = cumsum(headerRow)
    )

  mealTypeDF <- tables %>%
    filter(headerRow==1) %>%
    select(matchID,date) %>%
    rename(program = date)

  tables2 <- tables %>%
    filter(
      !(assistanceProgram %in% c('Total','Assistance Program')),
      !is.na(totalEligible)
    ) %>%
    left_join(mealTypeDF, by = 'matchID')

  # Extract school name & attendance factor from the html subheadings, then attach
  # a value to match them to tables
  htmlSubheadings <- rvest::html_nodes(html, '.sub-heading')
  subheading <- as.data.frame(rvest::html_text(htmlSubheadings)) %>%
    dplyr::rename(var = 1) %>%
    tidyr::separate(var, into = c('building','attendanceFactor'), sep = "\\(Attendance Factor ", extra = "merge") %>%
    dplyr::mutate(
      building = stringr::str_trim(building),
      attendanceFactor = as.numeric(gsub('[^0-9.]','',attendanceFactor))/100,
      matchID = row_number()
    )

  # merge and fix data types
  finalOut <- tables2 %>%
    dplyr::left_join(subheading, by = 'matchID') %>%
    dplyr::mutate_at(vars(totalEligible:totalClaims), as.numeric) %>%
    dplyr::mutate(
      date = as.POSIXct(date,format = '%m/%d/%Y', tz = 'UTC')
    ) %>%
    dplyr::relocate(building,program,attendanceFactor) %>%
    dplyr::select(-c(headerRow,matchID)) %>% 
    splitNameAndID(building)

  return(finalOut)

}

#' Import a Titan HTML report
#'
#' This function simplifies the process of importing and cleaning a report from
#' Titan
#'
#' @param inFile Path to the saved Titan HTML report
#' @return A data frame containing the content of the Titan Report
#' @export

titanImportHTML <- function(inFile) {
  df <- inFile %>%
    rvest::read_html() %>%
    rvest::html_table() %>%
    .[[1]] %>%
    janitor::clean_names(case='lower_camel')

  return(df)
}

#' Import a Titan HTML Production Report
#'
#' This function imports and cleans an HTML production records report from Titan
#'
#' @param inFile The path to a saved Titan Production Records Report
#' @param htmlListVars A vector containing the variables that you grouped the
#'   production records by. must be variables like "school" and "date"
#' @return a data frame containing the content of a Production records report
#' @export

titanImportProdRecs <- function(inFile,htmlListVars=c('school','date')) {

  htmlListVarsTotal <- length(htmlListVars)

  titanColNames <- c(
    'identifier','name','plannedReimbursable','plannedNonReimbursable','plannedTotal',
    'offered','servedReimbursable','servedNonReimbursable','servedTotal','servedCost',
    'discardedTotal','discardedPctOfOffered','discardedCost','subtotal','leftOverTotal',
    'leftOverPctOfOffered','leftOverCost','productionCost'
  )

  html <- read_html(inFile)

  # Extract tables
  tables <- html %>%
    rvest::html_nodes("table") %>%
    map_df(~rvest::html_table(.x, header = FALSE))

  # Extract unordered lists
  lists <- html %>%
    rvest::html_nodes("ul") %>%
    map_df(~{
      # Extract list items
      items <- .x %>%
        rvest::html_nodes("li") %>%
        rvest::html_text()
      # Convert list items to a data frame
      data.frame(value = items)
    })

  # So ChatGPT got us this far, creating two data sets. We just need to figure out
  # which one to attach to which table. To do that, we're going to ennumerate both
  # lists. Each associated list item should last for 2 values. Each table is
  # broken up by an empty line with a dollar value under "served," which we'll use
  # to break up the table

  listMerge <- lists %>%
    dplyr::mutate(
      id = ceiling(row_number()/htmlListVarsTotal)
    ) %>%
    group_by(id) %>%
    dplyr::mutate(
      colName = sprintf('v%s',row_number())
    ) %>%
    pivot_wider(
      names_from = colName,
      values_from = value
    ) %>%
    set_names(c('id',htmlListVars))

  if(any(grepl('date',htmlListVars))) {
    listMerge <- listMerge %>%
      dplyr::mutate(
        date = as.POSIXct(date, format = "%A, %B %d, %Y", tz = 'UTC')
      )
  }

  df <- tables %>%
    set_names(titanColNames) %>%
    dplyr::mutate(
      counter = ifelse((lag(identifier) == '' & lag(name) == '') | row_number() == 1,1,0),
      id = cumsum(counter)
    ) %>%
    dplyr::filter(
      !(discardedTotal == 'Discarded' & discardedCost == 'Discarded'),
      !(identifier == '' & name == ''),
      !(identifier == 'Identifier' & name == 'Name')
    ) %>%
    dplyr::left_join(listMerge, by = 'id') %>%
    dplyr::select(date,school,everything(),-c(counter,id)) %>%
    dplyr::mutate_at(vars(plannedReimbursable:productionCost), ~ as.numeric(gsub("[,$%]", "", .)))
  
  if(any(grepl('school',htmlListVars))) {
    df <- df %>% 
      splitNameAndID(school) %>% 
      rename(school = building)
  }

  return(df)

}

#' Import and clean the Warehouse transfers report
#'
#' This function imports the badly formatted Titan Warehouse Transfer Report and
#' moves the product and product identifier to the same row as the transfer
#' reports.
#'
#' @param inFile This is the directory of an HTML Warehouse Transfers Report
#' @return A data frame with the product and identifier values appropriately placed
#' @export

titanImportWHTransfers <- function(inFile) {
  step1 <- titanImportHTML(inFile) %>%
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

  df <- step1 %>%
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
    ) %>% 
    splitNameAndID(receivingBuilding) %>% 
    rename(
      receivingBuilding = building,
      receivingID = buildingID
    ) %>% 
    splitNameAndID(fulfillmentBuilding) %>% 
    rename(
      fulfillmentBuilding = building,
      fulfillmentID = buildingID
    )

  return(df)
}
















