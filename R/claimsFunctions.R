# Claims Functions. The functions in this sheet are special functions to help
# with our monthly claims cycle. They focus on reading data and appropriately
# formatting data for Titan uploads.

#' @importFrom dplyr arrange distinct filter group_by mutate mutate_all relocate
#'   rename row_number select summarise ungroup
#' @importFrom googlesheets4 as_sheets_id gs4_auth read_sheet
#' @importFrom janitor clean_names
#' @importFrom stringr str_extract str_to_lower
#' @importFrom readr read_csv
#' @importFrom tidyr pivot_longer separate uncount
#'
NULL

#' Import data stored in the CACFP attendance Google Sheets and simplify it down
#' to the CACFP ID & attendance value.
#'
#' This function imports and lightly cleans attendance data that we currently
#' store & manage inside of a Google sheet in a really wonky format. This piece
#' of scripting could very easily fail if we fail to maintain rigorous standards
#' around how we format those tables.
#'
#' @param sheetID The end of a google sheet url. Usually a 44'ish long
#'   character string that uniquely identifies a sheet.
#' @param dataSheet The sheet in the spreadsheet that contains the attendance
#'   data.
#' @return A cleaned up data frame with 3 columns - the school's cacfp id, date,
#'   and attendance
#' @export

mealReimbursementRates <- function() {

  githubDir <- 'https://raw.githubusercontent.com/holdind/bpsr/main/data/mealReimbursementRates.csv'

  reimbursementRates <- readr::read_csv(githubDir) %>%
    dplyr::mutate(
      endYear = ifelse(is.na(endYear),bpsr::fiscYearFromDate(Sys.Date()),endYear),
      #create vended & full-prep rows for each item labeled "all"
      expansion = ifelse(prep == 'all',2,1)
    ) %>%
    bpsr::tableExpanderByYear(startYear,endYear) %>%
    tidyr::uncount(expansion,.remove=F) %>%
    dplyr::group_by(fiscalYear,meal,progName) %>%
    dplyr::mutate(
      prep = ifelse(dplyr::row_number()==1 & prep == 'all','full-prep',prep),
      prep = ifelse(dplyr::row_number()==2 & prep == 'all','vended',prep)
    ) %>%
    dplyr::ungroup() %>%
    dplyr::relocate(fiscalYear,.before='meal') %>%
    dplyr::select(-expansion) %>%
    dplyr::arrange(fiscalYear,progName,prep,meal)

  return(reimbursementRates)

}

#' Import data stored in the CACFP attendance Google Sheets and simplify it down
#' to the CACFP ID & attendance value.
#'
#' This function imports and lightly cleans attendance data that we currently
#' store & manage inside of a Google sheet in a really wonky format. This piece
#' of scripting could very easily fail if we fail to maintain rigorous standards
#' around how we format those tables.
#'
#' @param sheetID The end of a google sheet url. Usually a 44'ish long
#'   character string that uniquely identifies a sheet.
#' @param dataSheet The sheet in the spreadsheet that contains the attendance
#'   data.
#' @return A cleaned up data frame with 3 columns - the school's cacfp id, date,
#'   and attendance
#' @export

readCACFPAttendance <- function(sheetID,dataSheet) {

  tokenPath = 'projects/f&ns/projects/fnclms/data/token.rds'

  googlesheets4::gs4_auth(email = TRUE, cache = tokenPath)

  # CACFP worksheet
  ssAttendance <- googlesheets4::as_sheets_id(sheetID)

  attendance <- googlesheets4::read_sheet(ssAttendance,sheet = dataSheet, skip = 1) %>%
    janitor::clean_names(case = 'lower_camel') %>%
    dplyr::mutate_all(as.character)

  attendanceCols <- colnames(attendance)
  dateCols <- attendanceCols[grepl('^x',attendanceCols)]

  attendance2 <- attendance %>%
    tidyr::pivot_longer(
      all_of(dateCols),
      names_to = 'date',
      values_to = 'attendance'
    ) %>%
    dplyr::mutate(
      date = stringr::str_extract(date, "[^_]*_[^_]*"),
      date = as.Date(date, format = 'x%m_%d'),
      attendance = as.numeric(gsub('[^0-9]','',attendance))
    ) %>%
    dplyr::filter(
      !is.na(attendance)
    ) %>%
    # Multiple programs can exist at the same school, so we want to sum up all
    # enrollment per day per program
    dplyr::group_by(date,cacfpId) %>%
    dplyr::summarise(
      attendance = sum(attendance)
    )

  return(attendance2)

}

#' Read the school-level attendance sheets that schools use for CACFP
#'
#' This function imports data from a Google sheet that tracks attendance at
#' schools, tallies the total attendance by program by day
#'
#' @param sheetID The end of a google sheet url. Usually a 44'ish long character
#'   string that uniquely identifies a sheet.
#' @param combinePrograms A boolean that indicates whether or not attendance
#'   should be combined accross sheets. Some programs use multiple sheets for 1
#'   program, in cases like these you'd want combine programs to be True. In
#'   cases where multiple sheets are used for multiple, distinct programs, this
#'   should be set to false to count each independently.
#' @return A cleaned up data frame with 3 columns - the school's cacfp id, date,
#'   and attendance
#' @export

readCACFPAttendanceCount <- function(sheetID,combinePrograms=T) {

  tokenPath = 'projects/f&ns/projects/fnclms/data/token.rds'

  googlesheets4::gs4_auth(email = TRUE, cache = tokenPath)

  ssDeets <- googlesheets4::as_sheets_id(sheetID)

  sheets <- googlesheets4::sheet_names(ssDeets)

  # remove example sheet from list of sheets to compile
  sheets <- sheets[!grepl('example',sheets,ignore.case=T)]

  # sheets <- sheets[!(sheets %in% discountSheets)]

  attendanceFin <- data.frame()

  for(sheet in sheets) {

    attendanceBase <- googlesheets4::read_sheet(
      ssDeets,sheet = sheet,
      skip = 5
    ) %>%
      janitor::clean_names(case='lower_camel')

    # Exit loop if there are no columns
    if(ncol(attendanceBase) == 0) {
      next
    }

    # Put the name at the top of the next column
    attendanceBase <- attendanceBase %>%
      dplyr::rename(name = 1) %>%
      dplyr::mutate_all(as.character)

    dateVars <- attendanceBase[,c(grepl('x[0-9]+_[0-9]+_[0-9]+|name',colnames(attendanceBase)))]

    # exit if there are no date vars
    if(length(dateVars)<=1) {
      next
    }

    attendanceClean <- dateVars %>%
      tidyr::pivot_longer(
        cols = -name,
        names_to = 'date',
        values_to = 'value'
      ) %>%
      tidyr::separate(date, sep='_', into=c('month','date','year')) %>%
      dplyr::mutate(
        # clean up date values
        year = ifelse(str_length(year)==2,sprintf('20%s',year),year),
        date = sprintf('%s_%s_%s',month,date,year),
        date = as.POSIXct(date,format='x%m_%d_%Y'),
        value = stringr::str_to_lower(value),
        program = sheet
      ) %>%
      dplyr::group_by(date,program) %>%
      dplyr::filter(value == 'present') %>%
      dplyr::distinct() %>%
      dplyr::summarise(total = n()) %>%
      dplyr::mutate(program = sheet)

    attendanceFin <- rbind(attendanceFin,attendanceClean)

  }

  # count programs separately?
  if(combinePrograms == T) {
    attendanceFinal <- attendanceFin %>%
      dplyr::group_by(date) %>%
      dplyr::summarise(total = sum(total)) %>%
      dplyr::mutate(program = 'full program')
  } else {
    attendanceFinal <- attendanceFin
  }

  return(attendanceFinal)

}
