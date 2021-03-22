library(ggplot2)
library(grid)      # multiple plots
library(gridExtra) # multiple plots
library(cowplot)   # multiple plots vertically aligned
library(grDevices) # To save the plots
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


###################################################################################################
#              BEST MODELS                                                                        #
###################################################################################################
do.report <- function(model, data, ncol, forcasted.days = 5L, forcasted.previous = 30L, 
                      alpha.risk = c(0.01, 0.05), nahead.risk = 5L, dateColumn = 1L, 
                      saveImages = list(logic = F, directory = getwd(), extension = 'png'), 
                      crises = NA){
  library(ggplot2)
  library(grid)      # multiple plots
  library(gridExtra) # multiple plots
  library(cowplot)   # multiple plots vertically aligned
  library(grDevices) # To save the plots
  
  checkSaveImage <- function(add) {
    if (saveImages$logic){
      dev.copy(png, paste0(saveImages$directory, '/', name, '_', add,'.' , saveImages$extension))
      dev.off()
    }
  }
  if (all(is.na(crises))){
    crises <- data.frame(matrix(c(c(as.character(min(data[, dateColumn]))), 
                        c(as.character(max(data[, dateColumn])))), ncol = 2))
    crises[, 1] <- as.Date(crises[, 1], format = '%Y-%m-%d')
    crises[, 2] <- as.Date(crises[, 2], format = '%Y-%m-%d')
    alpha.fill <- 0 # used to set the alpha channel for crises rectangles
  }
  else{
    alpha.fill <- 0.4
  }
  # Name of the column
  name <- colnames(data)[ncol]
  # Smoothed probabilities graph
  smooth.prob <- State(model)$SmoothProb[, 1, 2, drop = TRUE]
  cond.volat <- Volatility(model)
  cond.volat[cond.volat > 10] <- NA # adjust too high values
  pStates <- ggplot() + 
    geom_rect(aes(xmin = crises[, 1], xmax = crises[, 2], ymin = -Inf, ymax = Inf), 
              alpha = alpha.fill, fill = 'gray') + 
    geom_line(aes(x = data[, dateColumn], y = data[, ncol]), color = 'black', size = 0.4) + 
    geom_line(aes(x = data[, dateColumn], y = smooth.prob[-1] * 10), color = 'red', size = 0.9) + 
    theme_minimal() + 
    scale_x_date(date_minor_breaks = '1 year', name = '') + 
    scale_y_continuous(name = 'Returns', 
                       sec.axis = sec_axis(~./10 + 1, name = 'State', breaks = c(1L, 2L))) + 
    labs(title = paste("Regimes' switching -", name))
  pVol <- ggplot() + 
    geom_rect(aes(xmin = crises[, 1], xmax = crises[, 2], ymin = -Inf, ymax = Inf), 
              alpha = alpha.fill, fill = 'gray') + 
    geom_line(aes(x = data[, dateColumn], y = cond.volat), color = 'black', size = 0.4) + 
    geom_abline(intercept = 0, slope = 0, color = 'gray', size = 0.1) + 
    theme_minimal() + 
    scale_x_date(date_minor_breaks = '1 year') + 
    scale_y_continuous(limits = c(0, NA)) + 
    labs(y = 'Cond. Volatilities', x = 'Time')
  print(plot_grid(pStates, pVol, nrow = 2, align = 'v'))
  checkSaveImage('States')
  
  # FORECASTING
  predicted_days = forcasted.days
  pred <- predict(model, 
                  nahead = predicted_days, # Horizon of the prediction
                  ctr = list(nsim = 1e4L)) # Number of simulations(*)
  
  # Creates two scenarios
  statesFit <- ExtractStateFit(model)
  pred1 <- predict(statesFit[[1]], nahead = predicted_days)
  pred2 <- predict(statesFit[[2]], nahead = predicted_days)
  newdate <- c(data[seq(N-forcasted.previous, N), 1], seq(data[N, 1], data[N, 1] + 
                                            predicted_days - 1, by = 'days'))
  print(ggplot() + 
    geom_line(aes(x = newdate, 
                  y = c(Volatility(model)[seq(N-forcasted.previous, N)], 
                        rep(NA, predicted_days)), color = 'Cond. Vol.'), size = 0.6) + 
    geom_line(aes(x = newdate, 
                  y = c(rep(NA, forcasted.previous), Volatility(model)[N], 
                        as.numeric(pred1$vol)), colour = 'State 1'), size = 1.2) + 
    geom_line(aes(x = newdate, 
                  y = c(rep(NA, forcasted.previous), Volatility(model)[N], 
                        as.numeric(pred2$vol)), colour = 'State 2'), size = 1.2) + 
    theme_minimal() + 
    scale_x_date(date_minor_breaks = '1 month') + 
    scale_y_continuous(limits = c(0, NA)) + 
    scale_color_manual(values = c('Cond. Vol.' = 'black', 
                                  'State 1' = 'blue', 
                                  'State 2' = 'red')) + 
    labs(title = paste('Scenarios -', name), 
         subtitle = paste('next', predicted_days, 'days'), 
         y = 'Volatility', 
         x = 'Time', 
         color = 'Regimes'))
  checkSaveImage('Scenarios')
  
  # QUANTITATIVE RISK MANAGEMENT
  # Extract single-regime model results from the fitted object
  statesFit <- ExtractStateFit(model)
  # Value at Risck (VaR) of state 1
  risk1 <- Risk(statesFit[[1]], alpha = alpha.risk, do.es = T, ctr = list(nsim = 1e4L), 
                nahead = nahead.risk)
  # Value at Risck (VaR) of state 2
  risk2 <- Risk(statesFit[[2]], alpha = alpha.risk, do.es = T, ctr = list(nsim = 1e4L), 
                nahead = nahead.risk)
  # Combine the two states
  VaR <- cbind(risk1$VaR, risk2$VaR)
  # Compute expected-shortfall (ES) -> expected value below VaR
  ES <- cbind(risk1$ES, risk2$ES)
  plotVaR <- ggplot() + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 1], color = 'State 1')) + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 1], color = 'State 1', 
                   shape = as.character(alpha.risk[1]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 3], color = 'State 2')) + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 3], color = 'State 2', 
                   shape = as.character(alpha.risk[1]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 2], color = 'State 1'), 
              linetype = 'dashed') + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 2], color = 'State 1', 
                   shape = as.character(alpha.risk[2]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 4], color = 'State 2'), 
              linetype = 'dashed') + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = VaR[, 4], color = 'State 2', 
                   shape = as.character(alpha.risk[2]))) + 
    scale_color_manual(values = c('State 1' = 'blue', 
                                  'State 2' = 'red')) + 
    scale_shape_manual(values = setNames(c(1, 2), 
                c(as.character(alpha.risk[1]), as.character(alpha.risk[2])))) + 
    labs(title = paste('VaR -', name), 
         subtitle = paste('next', nahead.risk, 'days'), 
         y = 'Value at Risk', 
         x = 'Time', 
         color = 'Regimes', shape = 'Alpha') + 
    guides(color = F, shape = F) + 
    theme_minimal()
  plotES <- ggplot() + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = ES[, 1], color = 'State 1')) + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = ES[, 1], color = 'State 1', 
                   shape = as.character(alpha.risk[1]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = ES[, 3], color = 'State 2')) + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = ES[, 3], color = 'State 2', 
                   shape = as.character(alpha.risk[1]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = ES[, 2], color = 'State 1'), 
              linetype = 'dashed') + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = ES[, 2], color = 'State 1', 
                   shape = as.character(alpha.risk[2]))) + 
    geom_line(aes(x = seq(1L, nahead.risk, 1), y = ES[, 4], color = 'State 2'), 
              linetype = 'dashed') + 
    geom_point(aes(x = seq(1L, nahead.risk, 1), y = ES[, 4], color = 'State 2', 
                   shape = as.character(alpha.risk[2]))) + 
    scale_color_manual(values = c('State 1' = 'blue', 
                                  'State 2' = 'red')) + 
    scale_shape_manual(values = setNames(c(1, 2), 
                   c(as.character(alpha.risk[1]), as.character(alpha.risk[2])))) + 
    labs(title = paste('ES -', name), 
         subtitle = paste('next', nahead.risk, 'days'), 
         y = 'Expected-shortfall', 
         x = 'Time', 
         color = 'Regimes', shape = 'Alpha') + 
    theme_minimal()
  g <- ggplotGrob(plotES)$grobs # extract grobs from plotES to get the legend
  legend <- g[[which(sapply(g, function(x) x$name) == 'guide-box')]] # get legend grob
  lwidth <- sum(legend$width) # legend width
  print(grid.arrange(grobs = list(plotVaR, plotES + 
       guides(color = F, shape = F), legend), # remove legend from plotES
       ncol = 3, widths = unit.c((unit(1, 'npc') - lwidth) / 2, 
                                 (unit(1, 'npc') - lwidth) / 2, lwidth)))
  checkSaveImage('VaR_ES')
  
  # PREDICT DISTRIBUTION
  alpha <- alpha.risk
  VaR.x <- Risk(model, alpha = alpha, nahead = 1)$VaR
  # run PredPdf method in-sample
  mesh <- seq(VaR.x[1] - 0.5, -VaR.x[1] + 0.5, 0.01)
  pred.x <- PredPdf(object = model, x = mesh, do.its = F, log = F, nahead = 1, 
                    ctr = list(nsim = 1e4L))
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
    labs(title = paste('Value at Risk -', name), subtitle = 'forecast 1 day ahead', 
         x = '', y = 'Likelihood') + 
    scale_x_continuous(breaks = round(c(VaR.x), 2), minor_breaks = 
                         seq(as.integer(min(mesh)) - .5, as.integer(max(mesh)) + .5, .5)) + 
    theme_minimal()
  plotCum <- ggplot() +
    geom_line(aes(x = mesh, y = cumsum(pred.x)), size = 0.6) + 
    geom_vline(xintercept = VaR.x[2], color = 'blue', size = 0.9) + 
    geom_hline(yintercept = alpha[2] * 100, color = 'blue', size = 0.4, 
               linetype = 'dashed', alpha = 0.9) + 
    geom_vline(xintercept = VaR.x[1], color = 'red', size = 0.9) + 
    geom_hline(yintercept = alpha[1] * 100, color = 'red', size = 0.4, 
               linetype = 'dashed', alpha = 0.9) + 
    labs(x = 'Returns', y = 'Probability') + 
    scale_x_continuous(breaks = seq(as.integer(min(mesh)) - .5, 
            as.integer(max(mesh)) + .5, .5), minor_breaks = seq(as.integer(min(mesh)) - .5, 
            as.integer(max(mesh)) + .5, .5)) + 
    scale_y_continuous(breaks = seq(0, 100, 25), minor_breaks = seq(5, 95, 5)) +
    theme_minimal()
  print(plot_grid(plotLik, plotCum, nrow = 2, align = 'v'))
  checkSaveImage('VaR_distribution')
}

# main loop
var_dynamics  <- c('sARCH', 'sGARCH', 'eGARCH', 'gjrGARCH', 'tGARCH')
distributions <- c('norm', 'std', 'ged', 'snorm', 'sstd', 'sged')
best.models <- list(LL = list(model = list(), spec = list(), value = as.list(rep(-Inf, 6))), 
                    BIC = list(model = list(), spec = list(), value = as.list(rep(Inf, 6))), 
                    AIC = list(model = list(), spec = list(), value = as.list(rep(Inf, 6))))
idx <- 1
for (i in seq(2, 7)){
  print(paste0('Model on: ', cnames[i]))
  for (v in var_dynamics){
    for (d in distributions){
      print(paste0(cnames[i], ': ', v, ', ', d))
      spec <- CreateSpec(variance.spec = list(model = v),
                         distribution.spec = list(distribution = d),
                         switch.spec = list(do.mix = F, K = 2),
                         constraint.spec = list(fixed = NULL, regime.const = NULL))
      set.seed(123)
      model <- FitML(spec, StockReturns[, i], ctr = list(par0 = NULL, do.se = T, do.plm = F))
      # Check the best solution
      # LL
      if (model$loglik > best.models$LL$value[[idx]]){
        best.models$LL$value[[idx]] <- model$loglik
        best.models$LL$model[[idx]] <- model
        best.models$LL$spec[[idx]]  <- spec
      }
      # BIC
      if (BIC(model) < best.models$BIC$value[[idx]]){
        best.models$BIC$value[[idx]] <- BIC(model)
        best.models$BIC$model[[idx]] <- model
        best.models$BIC$spec[[idx]]  <- spec
      }
      # AIC
      if (AIC(model) < best.models$AIC$value[[idx]]){
        best.models$AIC$value[[idx]] <- AIC(model)
        best.models$AIC$model[[idx]] <- model
        best.models$AIC$spec[[idx]]  <- spec
      }
    }
  }
  idx <- idx + 1
}
get.best <- function(l, pos, metric = 'AIC'){
  # Gets the best model according to a metric from a 'best.models'-like list.
  # 'l'      -> list of the best solutions
  # 'pos'    -> integer, the column stock
  # 'metric' -> one among 'LL' (log-likelihood), 'AIC' (default), 'BIC'
  return(c(l[[metric]]$value[[pos]], l[[metric]]$spec[[pos]]$name))
}
# Print the best solution for each stock
for (i in seq(2, 7)){
  print(paste0(cnames[i], ':'))
  for (m in c('LL', 'BIC', 'AIC')){
    best <- get.best(best.models, i - 1, metric = m)
    print(paste0('    -> ', m, ' = ', best[1], ': ', best[2]))
  }
}
# Crisis periods
crises <- data.frame(
  matrix(c(c('1990-07-01', '1991-03-01', '2001-03-01', '2001-11-01'), 
           c('2007-12-01', '2009-06-01', '2010-05-01', '2012-03-01')), 
         byrow = T, ncol = 2)
)
crises[, 1] <- as.Date(crisis[, 1], format = '%Y-%m-%d')
crises[, 2] <- as.Date(crisis[, 2], format = '%Y-%m-%d')
# Plot the best model for each stock (according to AIC)
for (i in seq(2, 7)){
  do.report(best.models$AIC$model[[i - 1]], StockReturns, i, saveImages = list(logic = T, directory = 
            paste0(getwd(), '/Images'), extension = 'png'), crises = crises)
}

#####  ALTRE ASSUNZIONI
# Conditional Mean respect itself = 0 is confirmed 
attach(StockReturns)
acf(SP500)
acf(NASDAQ)
acf(FTSE)
acf(DAC)
acf(CAC)                         
                           
                           
####### SEMI HIDDEN MARKOV MODEL PARTENDO DA KMEANS
initialization <- kmeans(StockReturns[,2:6],2)
initialization
kmeans.means <- initialization$centers
plot(StockReturns[,2:6],col=initialization$cluster)
sigma1 <- cov(StockReturns[initialization$cluster==1,2:6]) #in the first state
sigma2 <- cov(StockReturns[initialization$cluster==2,2:6]) #in the second state
sigma1
sigma2

?hsmmspec

K <- 2
start.val <- hsmmspec(init = c(1,0), #hsmmspec instead of hmmspec, we use semimarkov
                      trans = matrix(c(0,1,1,0),byrow=T, nrow = K, ncol = K), #we have to fix the entry on the main diagonal equal to 0 because the sojourn distribution is shaped according a poisson
                      parms.emis = list(mu = list(kmeans.means[1,],kmeans.means[2,]), 
                                        sigma=list(sigma1,sigma2)),
                      sojourn = list(lambda = c(10,20),shift = c(1,1),type="poisson"), #is shifted by 1 because the poisson start from 1 usually(and you can't stay in a state 0 time)
                      dens.emission = dmvnorm.hsmm)
start.val

mod.hsmm.k2 <- hsmmfit(matrix(unlist(StockReturns[,2:6]),ncol=5),
                       start.val, mstep=mstep.mvnorm )#multivariate norm

View(mod.hsmm.k2)
AIC(mod.hsmm.k2)

mod.hsmm.k2$loglik 
mod.hsmm.k2$model$transition
mod.hmm.k2$model$parms.emission
mod.hsmm.k2$model$sojourn

par(mfrow=c(2,2))
plot(StockReturns[,2],col=mod.hmm.k2$yhat)
plot(StockReturns[,3],col=mod.hmm.k2$yhat)
plot(StockReturns[,4],col=mod.hmm.k2$yhat)
plot(StockReturns[,5],col=mod.hmm.k2$yhat)
plot(StockReturns[,6],col=mod.hmm.k2$yhat)

mod.hmm.k2$model$parms.emission$mu
mod.hmm.k2$model$parms.emission$sigma

cov2cor(mod.hmm.k2$model$parms.emission$sigma[[1]])
cov2cor(mod.hmm.k2$model$parms.emission$sigma[[2]])



                            
                         
                            
                           
