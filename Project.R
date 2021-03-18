library(ggplot2)

load("~/R/Time_Seties_Econometrics/DataForPresentation2021_timeseries.RData")
View(StockReturns)
StockReturns[, 1] <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")

#Starting from 02/01/1991
Stock1991 <- StockReturns[StockReturns$Date > as.Date("1990-12-31"), ]




# Multiple plot of the returns
cnames <- colnames(StockReturns)
plotList <- list()
for (i in seq(2, length(cnames))){
  if (i %in% c(2, 5)){
    ylab = "Returns"
  } else {
    ylab = ""
  }
  plotList[[i - 1]] <- ggplot(data = StockReturns, aes(x = Date, y = StockReturns[, i])) +
    geom_line() + geom_line(aes(y = 0), color = "blue", size = 1) + 
    geom_line(aes(y = mean(StockReturns[, i])), color = "red", linetype = 'dashed') + 
    labs(x = '', title = cnames[i], y = ylab) + 
    scale_x_date(date_breaks = '10 year', 
                 date_minor_breaks = '1 year', date_labels = '%Y', 
                 limits = c(as.Date('01/01/1989', format = "%d/%m/%Y"), 
                            as.Date('01/01/2019', format = "%d/%m/%Y")))
}
do.call(grid.arrange, c(plotList, nrow = 2))
# Assumption of 0 mean is confirmed


# Create a time series
class(StockReturns) # "data.frame"
df <- ts(StockReturns[, -1], frequency = 252)
rownames(df) <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")
plot(df)

# Conditional variance dynamics: ARCH, sGARCH, eGARCH, gjrGARCH, tGARCH
# Conditional distributions: norm, std (t-student), ged, snorm, sstd, sged

# 1) CreateSpec -> Create a model specification
# 2) FitML      -> Fit the model by maximum likelihood
# 3) predict    -> Compute the conditional volatility forecasts and the density forecasts
# 4) Risk       -> Compute the value-at-risk and expected-shortfall risk measures
