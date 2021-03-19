library(ggplot2)
library(gridExtra) # multiple plots
library(cowplot)   # multiple plots vertically aligned
library(MSGARCH)

load("~/R/Time_Seties_Econometrics/DataForPresentation2021_timeseries.RData")
View(StockReturns)
StockReturns[, 1] <- as.Date(StockReturns[, 1], format = "%d/%m/%Y")
N <- length(StockReturns[, 1])

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
# 5) PredPdf    -> Predict distribution

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
set.seed(123)
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
pStates <- ggplot(StockReturns, aes(x = Date)) + 
  geom_line(aes(y = data), color = 'black', size = 0.4) + 
  geom_line(aes(y = smooth.prob[-1] * 10), color = 'red', size = 0.9) + 
  theme_minimal() + 
  scale_x_date(date_minor_breaks = '1 year', name = '') + 
  scale_y_continuous(name = 'Returns', 
                     sec.axis = sec_axis(~./10, name = 'State')) + 
  labs(title = cnames[2])
pVol <- ggplot(StockReturns, aes(x = Date)) + 
  geom_line(aes(y = Volatility(model)), color = 'black', size = 0.4) + 
  geom_abline(intercept = 0, slope = 0, color = 'gray', size = 0.1) + 
  theme_minimal() + 
  scale_x_date(date_minor_breaks = '1 year') + 
  scale_y_continuous(limits = c(0, NA)) + 
  labs(y = 'Conditional Volatilities')
do.call(plot_grid, c(list(pStates, pVol), nrow = 2, align = 'v'))

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

# Creates two scenarios
statesFit <- ExtractStateFit(model)
pred1 <- predict(statesFit[[1]], nahead = predicted_days)
pred2 <- predict(statesFit[[2]], nahead = predicted_days)
newdate <- c(StockReturns[seq(N-60, N), 1], seq(StockReturns[N, 1], 
                    StockReturns[N, 1] + predicted_days - 1, by = 'days'))
ggplot() + 
  geom_line(aes(x = newdate, 
                y = c(Volatility(model)[seq(N-60, N)], rep(NA, predicted_days)), 
                color = 'Cond. Vol.'), 
            size = 0.6) + 
  geom_line(aes(x = newdate, 
                y = c(rep(NA, 60), Volatility(model)[N], as.numeric(pred1$vol)), 
                colour = 'State 1'), 
            size = 1.2) + 
  geom_line(aes(x = newdate, 
                y = c(rep(NA, 60), Volatility(model)[N], as.numeric(pred2$vol)), 
                colour = 'State 2'), 
            size = 1.2) + 
  theme_minimal() + 
  scale_x_date(date_minor_breaks = '1 month') + 
  scale_y_continuous(limits = c(0, NA)) + 
  scale_color_manual(values = c('Cond. Vol.' = 'black', 
                                'State 1' = 'blue', 
                                'State 2' = 'red')) + 
  labs(title = paste('Scenarios -', cnames[2]), 
       subtitle = paste('next', predicted_days, 'days'), 
       y = 'Volatility', 
       x = 'Date', 
       color = 'Regimes')


# 4) QUANTITATIVE RISK MANAGEMENT
# Extract single-regime model results from the fitted object
statesFit <- ExtractStateFit(model)
# Value at Risck (VaR) of state 1
risk1 <- Risk(statesFit[[1]],        # fitted object
              alpha = c(0.01, 0.05), # risk levels
              do.es = T,             # compute the expected-shortfall (default = T)
              ctr = list(            # control parameters
                nsim = 1e4L          # number of simulation done for estimation (default = 1e4L)
              ), 
              nahead = 5)            # forecast horizon
# Value at Risck (VaR) of state 2
risk2 <- Risk(statesFit[[2]], alpha = c(0.01, 0.05), nahead = 5)
# Combine the two states
VaR <- cbind(risk1$VaR, risk2$VaR)
colnames(VaR) <- c("State 1 (alpha=0.01)", "State 1 (alpha=0.05)", 
                   "State 2 (alpha=0.01)", "State 2 (alpha=0.05)")
VaR
# Hence, we are able to evaluate the risk exposure of an investment conditionally 
# on different regimes of the market -> create scenarios (using predict) in different 
# regimes.

# Compute expected-shortfall (ES) -> expected value below VaR
ES <- cbind(risk1$ES, risk2$ES)
colnames(ES) <- c("State 1 (alpha=0.01)", "State 1 (alpha=0.05)", 
                  "State 2 (alpha=0.01)", "State 2 (alpha=0.05)")
ES


# 5) PREDICT DISTRIBUTION
# run PredPdf method in-sample
mesh <- seq(-2, 2, 0.01)
pred.x <- PredPdf(object = model,  # fitted object
                  x = mesh,        # Used when do.its = FALSE (mesh grid where perform simulations)
                  do.its = F,      # if the in-sample predictive is returned (used if 'nahead' > 1)
                  log = F,         # if the log-density is returned. (Default: log = FALSE)
                  nahead = 1,
                  ctr = list(
                    nsim = 1e4L    # number of simulations
                  ))

alpha <- c(0.01, 0.05)
VaR.x <- Risk(model, alpha = alpha, nahead = 1)$VaR
plotLik <- ggplot() +
  geom_line(aes(x = mesh, y = pred.x), size = 0.6) + 
  geom_vline(xintercept = VaR.x[2], color = 'blue', size = 0.9) + 
  geom_label(aes(label = paste0('alpha = ', alpha[2])),  
             x = VaR.x[2], 
             y = max(pred.x) * 0.95,
             label.padding = unit(0.25, 'lines'), # rectangle size around the label
             color = 'blue') + 
  geom_vline(xintercept = VaR.x[1], color = 'red', size = 0.9) + 
  geom_label(aes(label = paste0('alpha = ', alpha[1])),  
             x = VaR.x[1], 
             y = max(pred.x) * 0.8, 
             label.padding = unit(0.25, 'lines'), # rectangle size around the label
             color = 'red') + 
  labs(title = paste('Value at Risk -', cnames[2]), subtitle = 'forecast 1 day ahead', 
       x = '', y = 'Liklihood') + 
  scale_x_continuous(breaks = round(c(VaR.x), 2), minor_breaks = round(seq(min(mesh), max(mesh), .5), 2)) + 
  theme_minimal()
plotCum <- ggplot() +
  geom_line(aes(x = mesh, y = cumsum(pred.x)), size = 0.6) + 
  geom_vline(xintercept = VaR.x[2], color = 'blue', size = 0.9) + 
  geom_hline(yintercept = 5, color = 'blue', size = 0.4, linetype = 'dashed', alpha = 0.9) + 
  geom_vline(xintercept = VaR.x[1], color = 'red', size = 0.9) + 
  geom_hline(yintercept = 1, color = 'red', size = 0.4, linetype = 'dashed', alpha = 0.9) + 
  labs(x = 'Conditional Volatility', y = 'Probability') + 
  scale_x_continuous(breaks = round(seq(min(mesh), max(mesh), 1), 2)) + 
  scale_y_continuous(breaks = seq(0, 100, 25), minor_breaks = seq(5, 95, 5)) +
  theme_minimal()
do.call(plot_grid, c(list(plotLik, plotCum), nrow = 2, align = 'v'))
