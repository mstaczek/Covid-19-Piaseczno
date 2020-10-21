fromSeptember <- read.csv("danePiaseczno_latest.csv")

fromSeptember <- fromSeptember %>%
  filter(Date >= "2020-09-01") %>%
  mutate(Date = as.Date(Date))


ggplot(fromSeptember, aes(x = Date, y = Confirmed - Cured, fill = Cured)) +
  geom_bar(stat="identity") +
  xlab("Date") +
  ylab("No. cases") + 
  labs(title = "Active cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = Confirmed, fill = Confirmed)) +
  geom_bar(stat="identity") +
  xlab("Day 0 = 1.9.2020") +
  ylab("No. cases") + 
  labs(title = "Confirmed cases in Piaseczno")

ggplot(fromSeptember, aes(x = Date, y = Cured, fill = Cured)) +
  geom_bar(stat="identity") +
  xlab("Day 0 = 1.9.2020") +
  ylab("No. cured cases") + 
  labs(title = "Cured cases in Piaseczno")


nc <- rev(fromSeptember$New_confirmed)
b <- cumsum(nc)
d <- c(rep(0,14), b[1:(length(b)-14)])
res <- b-d
res/19

fromSeptember <- fromSeptember %>%
  mutate("someECDC" = rev(res/18.3)) %>%
  filter(Date >= "2020-09-14")

ggplot(fromSeptember, aes(x = Date, y = someECDC, fill = someECDC)) +
  geom_bar(stat="identity") +
  xlab("Day 0 = 14.9.2020") +
  ylab("someECDC") + 
  labs(title = "ECDC - New cases/10k people in the previous 2 weeks")



last2Weeks <- head(fromSeptember$New_confirmed,  14)
print(paste("ECDC wskaźnik =", round(sum(last2Weeks)/18.2,1)))

