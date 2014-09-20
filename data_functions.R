# Data processing functions for the Long-Term FX Momentum Analyser (FXAA).

# Most of these functions are very basic, and the most trivial are presented as
# functions mainly for expository reasons. The functions included here are:
#   BidAskSpreads (bid ask spreads given bid and ask prices).
#   BootstrapT (t-statistic of Sharpe ratio using moving fixed-block bootstrap).
#   ExcessReturns (excess returns given spot and forward time series).
#   ForwardDiscounts (forward discounts from spot and forward time series).
#   SharpeRatio (annualised sharpe ratio from monthly prices).
#   SpotRateChanges (spot rate changes given spot prices).


BidAskSpreads = function(bid, ask) {
  # Calculates the bid-ask spread of currencies per period.
  
  # Arguments:
  # "bid" time series with log bid prices (xts).
  # "ask" time series with log ask prices (xts).
  
  # Returns: time series with the bid-ask spreads (xts).

  spreads = ask - bid

  # Need to adjust "GBP" (inverted quotation convention).
  spreads[ , "GBP"] = spreads[ , "GBP"] * (-1)
  spreads
}


BootstrapT = function(rets) {
  # Calculates the t-statistic of a Sharpe ratio by applying a moving
  # fixed-length block bootstrap.

  # Arguments: "rets" period log returns (xts).
  # Returns: estimated t-statistic (numeric).

  rets = na.omit(rets)

  # "tsboot" is a function from the "sandwich" package.
  boot.results = tsboot(
    tseries   = rets,

    statistic = function(x) {
      sqrt(12) * (mean(x, na.rm=TRUE) / sd(x, na.rm=TRUE))
    },

    R         = 100,		# Number of blocks to evaluate.
    sim       = "fixed",	# Fixed length blocks.

    # "l" is the length of each block, set to 100 observations less than the
    # time series subject to a floor of 50.
    l = max(50, length(rets) - 100) 
  )
  
  # "t0" is the original estimate of standard error of the whole time series,
  # "t" is the series of standard errors calculated on the sample blocks.
  boot.results$t0 / sd(boot.results$t)
}


ExcessReturns = function(spot, forward) {
  # Calculates excess returns from holding a currency for one period
  # quoted in the base currency (assuming Covered Interest Parity holds).

  # Arguments:
  # "spot" time series of log spot prices (xts).
  # "forward" time series of log forward prices (xts).

  # Returns: time series of excess returns (xts).

  lag(forward) - spot
}


ForwardDiscounts = function(spot, forward) {
  # Calculates the forward discount of an asset per period.

  # Arguments:
  # "spot" time series of log spot prices (xts).
  # "forward" time series of log forward prices (xts).

  # Returns: xts time series with the forward discounts. 

  forward - spot
}


SharpeRatio = function(rets) {
  # Calculates the sharpe ratio of a series of monthly returns.
  # Arguments: "rets" vector of monthly log returns (numeric).
  # Returns: the sharpe ratio (numeric).

  # Multiplied by sq. root 1200 to annualise and convert to percentage.
  sqrt(1200) * mean(rets, na.rm=TRUE) / sd(rets, na.rm=TRUE)
}


SpotRateChanges = function(spot) {
  # Calculates the spot rate changes of an asset.
  # Arguments: "spot" time series of log spot asset prices (xts).
  # Returns: time series with the spot rate changes (xts).

  diff(dat$spot)
}
