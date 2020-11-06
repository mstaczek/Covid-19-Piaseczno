library(rvest)
library(stringi)
library(dplyr)


### The few functions below gather all data from sites like this one:
# http://piaseczno.psse.waw.pl/aktualnosci-i-komunikaty/komunikaty
# and output them in a tibble.

# More comments near the end of the file


extractDataFromPage <- function(url) {
  
  webpage <- suppressWarnings(read_html(url))
  
  # Extract the data from the page
  all <- webpage %>%
    html_nodes(".arc_102") %>%
    html_nodes("li") %>%
    html_text()
  datesRaw <- webpage %>%
    html_nodes(".arc_102") %>%
    html_nodes("time") %>%
    html_text() 
  return(list(datesRaw=datesRaw, all=all))
}

getListsOfData <- function(urlbase) {
  rawData <- c()
  dates <- c()
  i = 1
  ended <- FALSE
  while(TRUE){
    url <- paste(urlbase, i, sep='')
    
    result = tryCatch({
      holderAllAndDates <- extractDataFromPage(url)
      
      dates <- c(dates, stri_replace_all_fixed(holderAllAndDates$datesRaw, ".", "-"))
      rawData <- c(rawData, holderAllAndDates$all[stri_count_regex(holderAllAndDates$all, "\\d+") > 0 ])
      
    }, warning = function(w) {
      # print("hi warning")
    }, error = function(e) {
      # print("hi error")
      print(e)
      ended <<- TRUE
      
    }, finally = {
      print(paste("Finished downloading file", i))
      if(ended == TRUE){
        break
      }
    })
    i <- i + 1
  }    
  print("Finished downloading all files.")
  return(list(rawData=rawData, dates=dates))
}

createDataFrame <- function(rawData, dates) {
  
  res <- data.frame(matrix(rawData, ncol = 8, byrow=TRUE))
  
  names(res) <- c("Quarantined", "Home_isolated", "Isolated", "Supervised",
                  "Hospitalized", "Confirmed", "Deceased", "Cured")
  
  # wrappers
  exFirst <- function(col) {as.integer(stri_extract_first_regex(col, "\\d+"))}
  exLast  <- function(col) {as.integer(stri_extract_last_regex(col, "\\d+"))}
  
  # extracting numbers from gathered data
  res %>% mutate(
    "Date" = as.Date(unique(dates),tryFormats = c("%d-%m-%Y")),
    
    "New_quarantined" = exLast(Quarantined),
    Quarantined = exFirst(Quarantined),
    
    "New_home_isolated" = exLast(`Home_isolated`),
    `Home_isolated`= exFirst(`Home_isolated`),
    
    Isolated = exFirst(Isolated),
    
    "New_supervised" = exLast(Supervised),
    Supervised = exFirst(Supervised),
    
    Hospitalized = stri_replace_first_regex(Hospitalized, "\\d+",""),
    "New_hospitalized" = exLast(Hospitalized),
    Hospitalized = exFirst(Hospitalized),
    
    "New_confirmed" = exLast(Confirmed),
    Confirmed = exFirst(Confirmed),
    
    Deceased = stri_replace_first_regex(Deceased, "\\d+",""),
    "New_deceased" = exLast(Deceased),
    Deceased = exFirst(Deceased),
    
    "New_cured" = exLast(Cured),
    Cured = exFirst(Cured)) %>% 
    
    select(Date, Confirmed, New_confirmed, Quarantined, New_quarantined, Deceased, New_deceased, 
           Cured, New_confirmed, New_cured, Deceased, New_deceased, Home_isolated, New_home_isolated, 
           Supervised, New_supervised, Hospitalized, New_hospitalized, Isolated) %>% # reorder columns
    tibble()
}

getData <- function() {
  urlbase <- "http://piaseczno.psse.waw.pl/aktualnosci-i-komunikaty/komunikaty?page_a4="
  
  holderRawAndDates <- suppressWarnings(getListsOfData(urlbase = urlbase))

  createDataFrame(rawData = holderRawAndDates$rawData, dates = holderRawAndDates$dates)
}

saveToCSV <- function(dataframe, path="") {
  if(exists("directoryToSaveCSV")){
    path <- directoryToSaveCSV
  }
  fileTimestamp <- stri_replace_all_fixed(format(Sys.time(), "%Y-%m-%d_%X"),":","-")
  pathWithFilename <- paste(path, "danePiaseczno_", fileTimestamp,".csv", sep="")
  pathWithFilenameLatest <- paste(path, "danePiaseczno_latest.csv", sep="")
  
  write.csv(dataframe, pathWithFilename)
  write.csv(dataframe, pathWithFilenameLatest)
  
  print(paste('Files saved successfully to', pathWithFilename,'.'))
  print(paste('Files saved successfully to', pathWithFilenameLatest,'.'))
}

# for plotting ECDC
ECDCcolumn <- function(df) {
  nc <- rev(df$New_confirmed)
  b <- cumsum(nc)
  d <- c(rep(0,14), b[1:(length(b)-14)])
  res <- b-d
  rev(res/19)
}

ECDCdeltacolumn <- function(df){
  ecdc <- ECDCcolumn(df)
  moved <- c(ecdc[2:length(ecdc)], 0)
  ecdc - moved
}

addECDCcolumns <- function(df) {
  df %>% mutate("ECDC" = ECDCcolumn(df),
           "ECDCdelta" = ECDCdeltacolumn(df))
}

# for plotting new lockdown rate 
NewLockdownRatecolumn <- function(df) {
  nc <- rev(df$New_confirmed)
  b <- cumsum(nc)
  d <- c(rep(0,7), b[1:(length(b)-7)])
  res <- b-d
  rev(res/(1.9*7))
}

addNewLockdownRatecolumns <- function(df) {
  df %>% mutate("NewLockdownRate" = NewLockdownRatecolumn(df))
}


# To get a ready-to-use tibble, use 'getData' function  
freshData <- getData()

# adds "ECDC", "ECDCdelta" and "NewLockdownRate" comulns
updatedDate <-addNewLockdownRatecolumns(addECDCcolumns(freshData))

# You may change the path below to set the default output directory
# or pass the desired path as a 'path' argument to 'saveToCSV' function

# directoryToSaveCSV <- "C:\\Path\\to\\directory\\"
saveToCSV(updatedDate)