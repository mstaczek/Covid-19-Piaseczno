library(ggplot2)
library(scales)

NewLockdownRatePlot <- function(df) {
  ggplot(df, aes(x = Date, y = NewLockdownRate, fill = NewLockdownRate)) +
    geom_hline(yintercept = 70, color="red1", size=1.3) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("New Rate") + 
    labs(title = "Lockdown at 70 (Mean new cases from last 7 days / 100k people)")+
    scale_y_continuous(n.breaks = 7, limits=c(0,max(70,df$NewLockdownRate)))+
    theme(legend.position = "none")
}

OldRatePlot <- function(df) {
  ggplot(df, aes(x = Date, y = ECDC, fill = ECDC)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("Rate") + 
    labs(title = "Old Rate - New cases from previous 14 days / 10k people")+
    theme(legend.position = "none")
}

OldRatedeltasPlot <- function(df) {
  ggplot(df, aes(x = Date, y = ECDCdelta, fill = ECDCdelta)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("Old Rate delta") + 
    labs(title = "Old Rate Changes from the the previous day")+
    theme(legend.position = "none")
}


ActiveCasesPlot <- function(df) {
  # Active
  ggplot(df, aes(x = Date, y = Confirmed - Cured, fill = Confirmed - Cured)) +
    geom_bar(stat="identity") +
    xlab("Date") +
    ylab("No. cases") + 
    labs(title = "Active cases in Piaseczno")+
    theme(legend.position = "none")
}

TotalConfirmedPlot <- function(df) {
  ggplot(df, aes(x = Date, y = Confirmed, fill = Confirmed)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "Confirmed cases in Piaseczno")+
    theme(legend.position = "none")
}

TotalCuredPlot <- function(df) {
  ggplot(df, aes(x = Date, y = Cured, fill = Cured)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "Cured cases in Piaseczno")+
    theme(legend.position = "none")
}

NewConfirmedPlot <- function(df) {
  ggplot(df, aes(x = Date, y = New_confirmed, fill = New_confirmed)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "New confirmed cases in Piaseczno")+
    theme(legend.position = "none")
}

NewCuredPlot <- function(df) {
  ggplot(df, aes(x = Date, y = New_cured, fill = New_cured)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "New cured cases in Piaseczno")+
    theme(legend.position = "none")
}

TotalDeceasedPlot <- function(df) {
  ggplot(df, aes(x = Date, y = Deceased, fill = Deceased)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. deceased") + 
    labs(title = "Deceased in Piaseczno")+
    theme(legend.position = "none")
}

QuarantinedPlot <- function(df) {
  ggplot(df, aes(x = Date, y = Quarantined, fill = Quarantined)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "Quarantined now in Piaseczno")+
    theme(legend.position = "none")
}

NewQuarantinedPlot <- function(df) {
  ggplot(df, aes(x = Date, y = New_quarantined, fill = New_quarantined)) +
    geom_bar(stat="identity") +
    xlab("Timeline") +
    ylab("No. cases") + 
    labs(title = "New quarantined in Piaseczno")+
    theme(legend.position = "none")
}



