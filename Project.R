library(ggplot2)
library(gridExtra)
library(MSGARCH)

load("~/R/Time_Seties_Econometrics/DataForPresentation2021_timeseries.RData")
View(StockReturns)
StockReturns[, 1] <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")

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


# Conditional variance dynamics: sARCH, sGARCH, eGARCH, gjrGARCH, tGARCH
# Conditional distributions: norm, std (t-student), ged, snorm, sstd, sged

# 1) CreateSpec -> Create a model specification
# 2) FitML      -> Fit the model by maximum likelihood
# 3) predict    -> Compute the conditional volatility forecasts and the density forecasts
# 4) Risk       -> Compute the value-at-risk and expected-shortfall risk measures

# 1) MODEL SPECIFICATION
# Specifies 2 states:
var_dynamic   <- c('eGARCH', 'eGARCH')
distributions <- c('std', 'std')
spec <- CreateSpec(variance.spec = list(model = var_dynamic),      # variance dynamic
                   distribution.spec = list(distribution = distributions), # distributions
                   switch.spec = list(do.mix = F, # a MSGARCH is modeled (default)
                                      K = NULL),  # n of components is decided by the len of variance.spec
                   constraint.spec = list(fixed = NULL,        # fixed parameters set by the user
                                          regime.const = NULL) # parameters set equal across regimes
                   ) 
# The relevant information is summarized with the summary method:
summary(spec)

# 2) MODEL ESTIMATION
# Using maximum likelihood:
data <- StockReturns[, 2]
model <- FitML(spec, data,     # specifications and data
               ctr = list(     # control parameters (different from FitMCMC())
                 par0 = NULL,  # starting parameters that overwrite the default starting parameter scheme
                 do.se = T,    # if standard errors should be computed (yes = T, no = F)
                 do.plm = F    # F = optimization step is performed without ensuring stationarity for the volatility processes
#                 OptimFUN =   # Custom optimization function (default: Broyden-Fletcher-Goldfarb-Shanno)
               ))
summary(model)
# All estimated parameters are significant at 95% confidence level.
# State 2 is the one with higher average volatility (higher 'nu').
# State 1 and 2 do not differ so much with respect to the response of previous values ('beta' is the same)
# [State 2 is a bit more responsive, as can be expected].
# Both the states are very consistent (high value on the main diagonal of the transiction matrix).
# Belonging to state 2 is a bit more probable (54.54%).
# LL: -8362.5148  -> Log-liklihood
# AIC: 16749.0297 -> Akaike information criterion
# BIC: 16830.2805 -> Bayesian information criterion

smooth.prob <- State(model)$SmoothProb[, 1, 2, drop = TRUE]
ggplot(StockReturns, aes(x = Date)) + 
  geom_line(aes(y = data), color = 'black', size = 0.4) + 
  geom_line(aes(y = smooth.prob[-1] * 10), color = 'red', size = 0.9) + 
  theme_minimal() + 
  scale_x_date(date_minor_breaks = '1 year') + 
  scale_y_continuous(name = 'Returns', 
                     sec.axis = sec_axis(~./10, name = 'State'))

# 3) FORECASTING
# We can forcas: (i) shape of the distribution of yT+h|IT, (ii) volatility.
predicted_days = 30L
pred <- predict(model, 
  nahead = predicted_days, # Horizon of the prediction
  ctr = list(nsim = 1e4L)) # Number of simulations(*)
# (*) For h = 1, the predictive volatility and density are available in closed form; 
# in other circumstances (if h > 1) forecasts are obtained by simulation, drawing 
# iteratively a new observation from the (one-step ahead) predictive distribution 
# and updating the K conditional variance processes accordingly.

# The predict() method returns an object of class ‘MSGARCH_FORECAST’ with two elements:
#     vol  -> numeric vector of length h, containing the standard deviations of the distributions 
#             yT+j|IT for j = 1, ..., h.
#     draw -> matrix of dimension h × nsim of the simulated MSGARCH process.

# Predicted volatility for the next 30 days:
print(pred$vol)


# 4) IVE RISK MANAGEMENT

