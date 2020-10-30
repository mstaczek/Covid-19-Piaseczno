source("Covid_Piaseczno.R")

library(dplyr)
library(ggplot2)

fromSeptember <- read.csv("danePiaseczno_latest.csv")

fromSeptember <- fromSeptember %>%
  filter(Date >= "2020-09-01") %>%
  mutate(Date = as.Date(Date))

# ECDC
last2Weeks <- head(fromSeptember$New_confirmed,  14)
print(paste("ECDC wskaźnik =", round(sum(last2Weeks)/19,1)))



ggplot(fromSeptember, aes(x = Date, y = Confirmed - Cured, fill = Confirmed - Cured)) +
  geom_bar(stat="identity") +
  xlab("Date") +
  ylab("No. cases") + 
  labs(title = "Active cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = Confirmed, fill = Confirmed)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "Confirmed cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = Cured, fill = Cured)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "Cured cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = New_confirmed, fill = New_confirmed)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "New_confirmed cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = New_cured, fill = New_cured)) +
  geom_bar(stat="identity") +
  xlab("Timeline") +
  ylab("No. cases") + 
  labs(title = "New cured cases in Piaseczno")

ECDC <- function(df) {
  nc <- rev(df$New_confirmed)
  b <- cumsum(nc)
  d <- c(rep(0,14), b[1:(length(b)-14)])
  res <- b-d
  rev(res/19)
}

fromSeptember %>%
  mutate("someECDC" = ECDC(fromSeptember)) %>%
  ggplot(aes(x = Date, y = someECDC, fill = someECDC)) +
  geom_bar(stat="identity") +
  xlab("Day 0 = last month") +
  ylab("ECDC") + 
  labs(title = "ECDC - New cases from previous 14 days / 10k people")

