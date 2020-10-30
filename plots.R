# setwd("Covid-19-Piaseczno")
source("Covid_Piaseczno.R")

library(dplyr)
library(ggplot2)

fromJune <- read.csv("danePiaseczno_latest.csv") %>% mutate(Date = as.Date(Date))

fromSeptember <- fromJune %>%  filter(Date >= "2020-09-01")

### ECDC latest
print(paste("ECDC wskaźnik =", round(sum(head(fromSeptember$New_confirmed,  14))/19,1)))

# ECDC
ggplot(fromJune, aes(x = Date, y = ECDC, fill = ECDC)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("ECDC") + 
  labs(title = "ECDC - New cases from previous 14 days / 10k people")

# ECDC
ggplot(fromJune, aes(x = Date, y = ECDCdelta, fill = ECDCdelta)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("ECDC delta") + 
  labs(title = "ECDC Changes from the the previous day")

# -- from September -- #

# Active
ggplot(fromSeptember, aes(x = Date, y = Confirmed - Cured, fill = Confirmed - Cured)) +
  geom_bar(stat="identity") +
  xlab("Date") +
  ylab("No. cases") + 
  labs(title = "Active cases in Piaseczno")

# Total Confirmed
ggplot(fromSeptember, aes(x = Date, y = Confirmed, fill = Confirmed)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "Confirmed cases in Piaseczno")

# Total Cured
ggplot(fromSeptember, aes(x = Date, y = Cured, fill = Cured)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "Cured cases in Piaseczno")

# New Confirmed
ggplot(fromSeptember, aes(x = Date, y = New_confirmed, fill = New_confirmed)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "New_confirmed cases in Piaseczno")

# New Cured
ggplot(fromSeptember, aes(x = Date, y = New_cured, fill = New_cured)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "New cured cases in Piaseczno")