library(ggplot2)

load("~/R/Time_Seties_Econometrics/DataForPresentation2021_timeseries.RData")
View(StockReturns)
StockReturns[, 1] <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")


ggplot(data = StockReturns, aes(x = Date, y = SP500)) +
  geom_line() +
  labs(x = "Date",
       y = "SP500",
       title = "Returns",
       subtitle = "from 26/11/1990 to 22/06/2016") + 
  scale_x_date(date_breaks = '5 year', 
               date_minor_breaks = '1 year', date_labels = '%Y', 
               limits = c(as.Date('01/01/1990', format = "%d/%m/%Y"), NA))


class(StockReturns) # "data.frame"
df <- ts(StockReturns[, -1], frequency = 252)
rownames(df) <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")
plot(df)
