# Titan Functions. These functions are intended to help address some of the
# shortcomings of the Titan system, like only producing reports in html and
# doing other psychotic practices like formatting reports real badly. I will
# also use it to fix the terrible practice of building the word DISCONTINUED
# into keys

#' @import tidyverse janitor
NULL

#' Imports Ace Endico Reports listed in a directory as well as the allowance
#' sheets.
#'
#' This function pulls in a collection of processed Ace Endico reports
#'
#' @param directory A directory where the Ace Endico Reports live.
#' @return Two data sets, one containing all items in the Ace Endico reports,
#'   one containing all of the allowances that are listed on sheets 2-5 of an
#'   Ace Endico Report.
#' @export

loadEndicoReports <- function(directory) {
  
  fileNames <- list.files(
    path = directory,
    pattern = 'velocity'
  )
  
  fileNames <- fileNames[!grepl('\\~',fileNames)]
  
  
  aeRep <- data.frame()
  aeAllowanceDF <- data.frame() 
  
  byoColNames <- c(
    'transactionID', 'schoolState', 'raNumber','raName',
    'schoolName', 'dropMe', 'creationDate', 'invDate', 'invNbr',
    'productDesc', 'productNbr', 'qty', 'discountPerCase', 'discount',
    'color'
  )
  
  for(f in fileNames) {
    
    print(f)
    
    aeFileDir <- sprintf('%s/%s',directory,f)
    
    # Read in all of the sheet names We'll need all of them. The first is the core
    # data set, while the remaining 5 are all necessary for flagging allowances.
    aeSheetList <- readxl::excel_sheets(
      sprintf('%s/%s',directory,f)
    )
    
    aeFile <- readxl::read_excel(aeFileDir, sheet = aeSheetList[1]) %>% 
      janitor::clean_names(case='lower_camel') %>% 
      mutate(report = f)
    
    
    # Read in all of the allowance sheets, add their color, and then add them to
    # the Allowance Data Frame
    for(sheet in aeSheetList[2:length(aeSheetList)]) {
      
      # There are 2 different formats for these sheets, because these shitheads
      # couldn't make things easy. To make them match, we'll have to change the
      # heading names for Butterball/LandOLakes/Rich Chicks (blue/yellow/orange)
      # to the headings for Michaels & RedGold. Exciting!
      
      print(sheet)
      
      allowanceColor <- regmatches(sheet, gregexpr("\\((.*?)\\)", sheet))[[1]]
      allowanceColor <- stringr::str_to_lower(gsub("[[:punct:]]", "", allowanceColor))
      
      aeAllowanceSheet = readxl::read_excel(aeFileDir, sheet = sheet) %>% 
        janitor::clean_names(case='lower_camel') %>% 
        mutate(color = allowanceColor)
      
      if(allowanceColor %in% c('blue','orange','yellow')) {
        
        aeAllowanceSheet <- aeAllowanceSheet %>% 
          rename_all(~byoColNames) %>% 
          janitor::clean_names(case='lower_camel') %>% 
          select(-dropMe)
        
      } else if (allowanceColor %in% c('red','green')) {
        
        aeAllowanceSheet <- aeAllowanceSheet %>% 
          mutate(
            discount = discount * -1,
            discountPerCase = discount/qty
          )
        
      }
      
      aeAllowanceDF <- aeAllowanceDF %>% plyr::rbind.fill(aeAllowanceSheet)
    }
    
    # append the file to the file dataset
    aeRep <- aeRep %>% 
      plyr::rbind.fill(aeFile)
    
  }
  
  return(list(aeRep,aeAllowanceDF))
  
}












