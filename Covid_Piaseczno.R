library(rvest)
library(stringi)
library(dplyr)
library(ggplot2)


### The few functions below gather all data from sites like this one:
# http://piaseczno.psse.waw.pl/aktualnosci-i-komunikaty/komunikaty
# and output them in a tibble.

# More comments near the end of the file


extractDataFromPage <- function(url) {
  
  webpage <- suppressWarnings(read_html(url))
  
  # Extract the data from the page
  all <- webpage %>%
    html_nodes("li") %>%
    html_text()
  datesRaw <- webpage %>%
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
  
  write.csv(dataframe, pathWithFilename)
  
  print(paste('Files saved successfully to', pathWithFilename,'.'))
}


# To get a ready-to-use tibble, use 'getData' function  
freshData <- getData()

# You may change the path below to set the default output directory
# or pass the desired path as a 'path' argument to 'saveToCSV' function

# directoryToSaveCSV <- "C:\\Path\\to\\directory\\"

saveToCSV(freshData)



# Some simple grey plots with ggplot2

ggplot(freshData, aes(x = Date, y = Confirmed - Cured)) +
  geom_bar(stat="identity", fill = "#949494") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "Active cases in Piaseczno")


ggplot(freshData, aes(x = Date, y = Confirmed)) +
  geom_bar(stat="identity", fill = "#949494") +
  xlab("Timeline") +
  ylab("Total confirmed cases") + 
  labs(title = "Confirmed cases in Piaseczno")


ggplot(freshData, aes(x = Date, y = New_confirmed)) +
  geom_bar(stat="identity", fill = "#949494") +
  xlab("Timeline") +
  ylab("New confirmed cases") + 
  labs(title = "New confirmed cases in Piaseczno")
