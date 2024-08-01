# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @importFrom dplyr filter group_by left_join lag mutate mutate_at relocate
#'   rename row_number select summarise ungroup
#' @importFrom janitor clean_names
#' @importFrom magrittr %>% set_names
#' @importFrom purrr map_df
#' @importFrom readr read_csv
#' @importFrom rvest html_nodes html_table html_text read_html
#' @importFrom stringr str_count str_extract str_trim
#' @importFrom tidyr pivot_longer pivot_wider separate
#' @importFrom tools file_path_sans_ext
#'
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
#' convention, this command will split those items and produce separate line
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
    tidyr::separate({{ var }}, into = paste0('var',seq(1:max(.$totSplits)+1)),sep= ' & ') %>%
    tidyr::pivot_longer(
      cols = paste0('var',seq(1:max(.$totSplits)+1)),
      values_to = varName,
      values_drop_na = T
    ) %>%
    dplyr::select(-totSplits,-name)

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
    tidyr::separate(
      {{ var }}, into = c(strTotal,strUnits),
      extra = 'merge',
      sep = ' '
    ) %>%
    dplyr::mutate({{ nameTotal }} := as.numeric({{ nameTotal }}))

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
  outFile <- titanImportWHTransfers(inFile)

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

#' Make specific adjustments to an Edit Check File's attendance and enrollment
#' and adjusts some school data.
#'
#' This function adjusts enrollment and attendance on an Edit Check report. Some
#' schools also need to have their meals assigned to other schools, and this
#' does that as well. It uses a couple of tables in my GitHub to accomplish
#' this, then adjusts some values throughout the EC report based on the values
#' of those tables.
#'
#' @param ec an edit check report being imported to R
#' @return an edit check report with attendance values properly adjusted and
#'   school designations appropriately aligned
#' @export

titanEditCheckAFCleaner <- function(ec) {

  gitHubAdjustments <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/titanAttendanceEnrollmentCorrection.csv'
  gitHubSchoolAligners <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/titanSchoolAligner.csv'

  supTAEAdjustments <- readr::read_csv(gitHubAdjustments, col_types='ddccdd') %>%
    bpsr::tableExpanderByYear(startYear,endYear) %>%
    dplyr::select(-schname)

  supTSchAligner <- readr::read_csv(gitHubSchoolAligners, col_types='ddcccc') %>%
    bpsr::tableExpanderByYear(startYear,endYear) %>%
    dplyr::select(-c(schname,newSchname))

  ec <- ec %>%
    dplyr::mutate(
      month = str_remove(bpsr::yyyymmString(date),"\\-"),
      fiscalYear = bpsr::fiscYearFromDate(date)
    )

  # Merge in and correct
  ec2 <- ec %>%
    dplyr::left_join(
      supTSchAligner,
      by = c('fiscalYear','buildingID'='id')
    ) %>%
    dplyr::mutate(
      building = ifelse(!is.na(newID)&newID!=buildingID,NA,building),
      buildingID = ifelse(is.na(newID),buildingID,newID)
    ) %>%
    dplyr::group_by(fiscalYear,buildingID,date,program) %>%
    dplyr::mutate(
      totalEligible = sum(totalEligible),
      totEligibleTimesAF = sum(totEligibleTimesAF),
      dayFraction = 1/n()
    ) %>%
    # fix building codes
    dplyr::group_by(fiscalYear,buildingID) %>%
    dplyr::mutate(building = max(building,na.rm=T))

  # Sum up the claims, enrollment, attendance over the course of the month
  ec3 <- ec2 %>%
    dplyr::group_by(fiscalYear,buildingID,building,date,program) %>%
    dplyr::summarise(
      totalClaims = sum(totalClaims),
      enrollment = max(totalEligible),
      attendance = max(totEligibleTimesAF)
    ) %>%
    dplyr::ungroup()

  # Corrects enrollment and attendance by overriding it in some spot
  ec4 <- ec3 %>%
    dplyr::left_join(supTAEAdjustments, by = c('fiscalYear','buildingID' = 'id')) %>%
    dplyr::mutate(
      attendance = ifelse(is.na(addToAttendance),attendance,attendance+addToAttendance),
      enrollment = ifelse(is.na(addToEnrollment),enrollment,enrollment+addToEnrollment)
    ) %>%
    dplyr::select(-c(addToEnrollment,addToAttendance))

  return(ec4)

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
    magrittr::set_names(titanColNames)

  # This prepares the data set by flagging rows with the school name in them, then
  # creates a running matchID column that will allow us to match school name in
  # later. It also drops data from a summary table at the end of the HTML report.

  df1 <- tables %>%
    dplyr::mutate(
      headerCol = ifelse(
        dplyr::row_number() == 1 | lag(date) == 'Total',1,0
      ),
      matchID = cumsum(headerCol)
    ) %>%
    dplyr::filter(!is.na(cashExpected))

  # school names are built into the first of 2 header rows. This table extract
  # them to merge onto the rows below.

  schNameMatch <- df1 %>%
    dplyr::filter(headerCol==1) %>%
    dplyr::select(date,matchID) %>%
    tidyr::separate(date, sep = ':', into = c('buildingID','building')) %>%
    dplyr::mutate(
      building = stringr::str_trim(building)
    )

  # This final block merges in school names, moves them to the front of the data
  # frame, drops variables created for this process, drops header column rows, and
  # then, finally, converts our string dollar values into numeric & date into
  # POSIXct

  dfFin <- df1 %>%
    dplyr::left_join(schNameMatch, by = 'matchID') %>%
    dplyr::relocate(buildingID, building) %>%
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
    purrr::map_df(~rvest::html_table(.x, header = FALSE))  %>%
    dplyr::select(1:5) %>%
    magrittr::set_names(colNames) %>%
    dplyr::mutate(
      headerRow = ifelse(assistanceProgram == 'Assistance Program',1,0),
      matchID = cumsum(headerRow)
    )

  mealTypeDF <- tables %>%
    dplyr::filter(headerRow==1) %>%
    dplyr::select(matchID,date) %>%
    dplyr::rename(program = date)

  tables2 <- tables %>%
    dplyr::filter(
      !(assistanceProgram %in% c('Total','Assistance Program')),
      !is.na(totalEligible)
    ) %>%
    dplyr::left_join(mealTypeDF, by = 'matchID')

  # Extract school name & attendance factor from the html subheadings, then attach
  # a value to match them to tables
  htmlSubheadings <- rvest::html_nodes(html, '.sub-heading')
  subheading <- as.data.frame(rvest::html_text(htmlSubheadings)) %>%
    dplyr::rename(var = 1) %>%
    tidyr::separate(var, into = c('building','attendanceFactor'), sep = "\\(Attendance Factor ", extra = "merge") %>%
    dplyr::mutate(
      building = stringr::str_trim(building),
      attendanceFactor = as.numeric(gsub('[^0-9.]','',attendanceFactor))/100,
      matchID = dplyr::row_number()
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
  
  html <- rvest::read_html(inFile)
  
  # Extract tables
  tables <- html %>%
    rvest::html_nodes("table") %>%
    purrr::map_df(~rvest::html_table(.x, header = FALSE))
  
  # Extract unordered lists
  lists <- html %>%
    rvest::html_nodes("ul") %>%
    purrr::map_df(~{
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
      id = ceiling(dplyr::row_number()/htmlListVarsTotal)
    ) %>%
    group_by(id) %>%
    dplyr::mutate(
      colName = sprintf('v%s',dplyr::row_number())
    ) %>%
    tidyr::pivot_wider(
      names_from = colName,
      values_from = value
    ) %>%
    magrittr::set_names(c('id',htmlListVars))
  
  if(any(grepl('date',htmlListVars))) {
    listMerge <- listMerge %>%
      dplyr::mutate(
        date = as.POSIXct(date, format = "%A, %B %d, %Y", tz = 'UTC')
      )
  }
  
  df <- tables %>%
    magrittr::set_names(titanColNames) %>%
    dplyr::mutate(
      counter = ifelse((dplyr::lag(identifier) == '' & dplyr::lag(name) == '') | row_number() == 1,1,0),
      id = cumsum(counter)
    ) %>%
    dplyr::filter(
      !(discardedTotal == 'Discarded' & discardedCost == 'Discarded'),
      !(identifier == '' & name == ''),
      !(identifier == 'Identifier' & name == 'Name')
    ) %>%
    dplyr::left_join(listMerge, by = 'id') %>%
    dplyr::select(htmlListVars,everything(),-c(counter,id)) %>%
    dplyr::mutate_at(vars(plannedReimbursable:productionCost), ~ as.numeric(gsub("[,$%]", "", .))) %>% 
    # extract and create a column containing the unit
    dplyr::mutate(
      unit = str_extract(name, "\\(([^()]|\\([^()]*\\))*\\)$") %>% 
        str_replace("^\\((.*)\\)$", "\\1"),
      name = str_remove(name, "\\s*\\(([^()]|\\([^()]*\\))*\\)$")
    ) %>% 
    dplyr::relocate(unit,.after=name)
  
  if(any(htmlListVars=='school')) {
    df <- df %>%
      splitNameAndID(school) %>%
      dplyr::rename(school = building)
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
    dplyr::mutate(
      internalHeader = ifelse(
        expectedDate == quantity,1,0
      ),
      idGrouping = cumsum(internalHeader)
    )

  idTable <- step1 %>%
    dplyr::filter(internalHeader == 1) %>%
    dplyr::select(idGrouping,receivingBuilding) %>%
    dplyr::rename(headerString = receivingBuilding) %>%
    dplyr::mutate(
      identifier = stringr::str_extract(headerString,'\\(([^)]+)\\)'),
      identifier = removeDiscontinued(identifier),
      identifier = stringr::str_trim(gsub('[[:punct:] ]+','',identifier)),

      product = sub('.*?\\)','',headerString),
      product = sub('^\\)','',product),
      product = stringr::str_trim(product),

    ) %>%
    dplyr::select(-headerString)

  df <- step1 %>%
    dplyr::filter(internalHeader == 0) %>%
    dplyr::left_join(
      idTable, by = 'idGrouping'
    ) %>%
    dplyr::mutate(
      expectedDate = as.POSIXct(expectedDate, format = '%b %d, %Y', tz = 'UTC'),
      quantity = as.numeric(quantity)
    ) %>%
    dplyr::select(
      -internalHeader,-idGrouping
    ) %>%
    splitNameAndID(receivingBuilding) %>%
    dplyr::rename(
      receivingBuilding = building,
      receivingID = buildingID
    ) %>%
    splitNameAndID(fulfillmentBuilding) %>%
    dplyr::rename(
      fulfillmentBuilding = building,
      fulfillmentID = buildingID
    )

  return(df)
}
















