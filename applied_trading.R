# Applied trading functions for "Long-Term FX Momentum Analyser (FXAA)".

# Functions contained:
  # StratCorr: correlation between returns of two strategies given frequency.
  # MeanSpreads: mean monthly bid-ask spreads in basis points.
  # PortfolioTurnover: the average turnover per month of a portfolio.
  #
  # PortfolioSpreads: mean difference between bid-ask spreads of currencies in
  #  a portfolio and mean for the total set of currencies.
  #
  # MomCorr: correlation matrix between momentum strategy variations.
  # CountCurrencies: counts number of currencies in data set over time.
  # ClassifyCurrencies: looks up currency classifications.
  # DescribeStats: descriptive statistics of currency data set.
  # IndividualReturns: returns from trading momentum in individual currencies.
  # LongTermReturns: returns of momentum strategies by number of months holding.
  # MovingAvgReturns: moving average returns from monthly returns series. 
  #
  # PortfolioBelongings: percentage of a) total trading months and b) available
  #   months for a currency, that a currency is part of the long and short
  #   portfolios.
  #
  # RecursiveMomReturns: returns of momentum strategies which have formation and
  #  holding period selected based on recent performance of strategy variations.
  #
  # SubsetReturns: momentum trading returns given random sample of currencies
  #   with restrictions on sample size. 

StratCorr = function(ts.x, ts.y, freq=1) {
  # Calculates the correlation between the log returns of two strategies for
  # a given frequency.

  # Arguments:
  # "ts.x" the monthly log returns of the first strategy (xts).
  # "ts.y" the monthly log returns of the first strategy (xts).
  # "freq" the frequency over which to calculate correlations (e.g. 1 for
  #   monthly, 3 for quarterly, 12 for annual) (integer).

  # Returns: the correlation given frequency (numeric). 

  as.numeric(cor(use="complete",
    filter(ts.x, rep(1 / freq, freq), sides=1),
    filter(ts.y, rep(1 / freq, freq), sides=1)
  ))
}


MeanSpreads = function(spot.bid, spot.ask, forward.bid, forward.ask) {
  # Calculates mean monthly bid-ask spreads in basis points.

  # Arguments:
  # "spot bid" time series of bid spot prices per currency (xts).
  # "spot ask" time series of bid spot prices per currency (xts).
  # "forward bid" time series of bid spot prices per currency (xts).
  # "forward ask" time series of bid spot prices per currency (xts).

  # Returns list with:
  # "$spot" time series of spot bid-ask spreads in basis points (xts).
  # "$forward" time series of forward bid-ask spreads in basis points (xts).

  spreads = list(
    spot = BidAskSpreads(spot.bid, spot.ask),
    forward = BidAskSpreads(forward.bid, forward.ask)
  )

  # Note: multiply by 10k to convert to basis points.
  spreads$spot[ , 1] = rowMeans(spreads$spot, na.rm = TRUE) * 10000
  spreads$forward[ , 1] = rowMeans(spreads$forward, na.rm = TRUE) * 10000
  list(spot = spreads$spot[ , 1], forward = spreads$forward[ , 1])
}


PortfolioTurnover = function(allocs, portf) {
  # Calculates the average turnover per month of a portfolio.

  # Arguments:
  # "allocs" time series alllocations of currencies to portfolios (xts).
  # "portf" the portfolio index (integer).

  # Returns: mean monthly turnover for the portfolio (numeric).

  1 - mean(na.rm=TRUE,
    rowSums((allocs == portf) & (lag(allocs) == portf), na.rm=TRUE) /
    rowSums(allocs == portf, na.rm=TRUE)
  )
}


PortfolioSpreads = function(allocs, spreads, portf) {
  # Calculates the mean difference between the bid-ask spreads of
  # the assets in a portfolio and the average bid-ask spread of all
  # currencies available in the period.

  # Arguments:
  # "allocs" time series alllocations of currencies to portfolios (xts).
  # "spreads" time series of spot bid-ask spreads per currency (xts).
  # "portf" the portfolio index (integer).

  # Returns: mean spread difference (numeric).

  mean(na.rm=TRUE,   # Times 10,000 to express in basis points.
    rowMeans(ifelse(data.frame(allocs) == portf, spreads, NA), na.rm=TRUE)
    - rowMeans(spreads, na.rm=TRUE)
  )
}


MomCorr = function(fps, hps, fstat, vstat, fees, ports, dat, timer="", freq=1) {
  # Produces a matrix of correlations between different FX momentum strategies
  # defined by their formation- and holding periods.

  # Arguments:
  # "fp" formation period in months (integer)
  # "hp" holding period in months (integer)
  # "fstat" the name of the statistic used to form portfolios (character)
  # "vstat" the name of the statistic used to compute returns (character)
  # "fees" the effective spread used to estimate fees (numeric)
  # "dat" list of xts time series with data on:
  #    "$spot" spot prices (xts)
  #    "$spot.bid" spot bid prices (xts)
  #    "$spot.ask" spot ask prices (xts)
  #    "$forward.bid" forward bid prices (xts)
  #    "$forward.ask" forward ask prices (xts)
  # "timer" xts time index for subsetting the data (character)
  # "freq" data frequency for the correlation calculation, e.g. 1 implies
  #   monthly frequency, 12 annual (integer)

  # Results: "fps*hps" by "fps*hps" upper triangle of correlations (matrix)

  results = lapply(fps, function(fp) {
    lapply(hps, function(hp) {

      memoizedCall(dirs=c(project.tag, "MomResults"), MomResults, fees=fees,
        memoizedCall(dirs=c(project.tag, "TradeMom"), TradeMom, fp, hp, fstat, vstat, ports, dat)[timer]
      )$mag.returns

    })
  })

  results = Reduce(cbind, Reduce(cbind, results))
  output = cor(filter(results, rep(1 / freq, freq), sides=1),, use="complete")
  ifelse(upper.tri(output), output, NA)   # Return only the upper triangle.
}


CountCurrencies = function(dat) {
  # Calculates the number of currencies on with data per time series period.
  # Arguments: "dat" time series with market data on currencies (xts).
  # Returns: count of currencies on which there is data per period (xts).

  as.xts(rowSums(is.finite(dat)), order.by=index(dat))
}


ClassifyCurrencies = function(dat, classifications) {
  # Matches (integer) classifications with currency data per ccy.

  # Arguments:
  # "dat" price data on currencies over time (xts).
  # "classifications" classification, integer, of each ccy per period (xts).

  # Returns: classification per currency per period (xts).
 
  dat = dat$spot[index(classifications), colnames(classifications)]
  dat[] = ifelse(is.finite(dat), classifications, NA)
  dat
}


DescribeStats = function(dat) {
  # Computes descriptive statistics for a currency data set.

  # Arguments:
  # "dat" list of xts time series with data on:
  #   "$spot" spot prices (xts)
  #   "$spot.bid" spot bid prices (xts)
  #   "$spot.ask" spot ask prices (xts)
  #   "$forward.bid" forward bid prices (xts)
  #   "$forward.ask" forward ask prices (xts)

  # Returns:descriptive statistics per currency (data frame).

  # Calculate the first and last date of data for every currency.
  symbols = colnames(dat$spot)

  date.ranges = sapply(symbols, function(symbol) {
    date.indices = index(na.omit(dat$spot[ , symbol]))

    c(start = as.character(min(date.indices)),
      end   = as.character(max(date.indices))
    )
  })

  BASpreads = function(dat) {
  # Convenience wrapper for BidAskSpreads.
    BidAskSpreads(bid=dat$spot.bid, ask=dat$spot.ask)
  }

  ExRet = function(dat) {
  # Convenience wrapper for ExcessReturns.
    ExcessReturns(dat$spot, dat$forward)
  }

  FwDisc = function(dat) {
  # Convenience wrapper for ExcessReturns.
    ForwardDiscounts(dat$spot, dat$forward)
  }

  # Calculate the selected statistics
  statistics = lapply(
    # Define the combination of time series and descriptive statistics.
    list(
      list(basis = ExRet,    stats = c(mean, sd, min, max)),
      list(basis = FwDisc, stats = c(mean, sd, min, max)),
      list(basis = BASpreads,    stats = c(mean, sd, min, max))
    ),
    # Apply each statistic to the relevant time series column (per currency).
    function(x) {
      sapply(x$stats, function(stat) {
        apply(x$basis(dat), 2, stat, na.rm=TRUE)
      })
    }
  )

  statistics = Reduce(cbind, statistics)
  # Convert to a data frame and assign appropriate column names.
  output = data.frame(t(date.ranges), statistics, stringsAsFactors=FALSE)

  colnames(output) = c(
    'Start',
    'End',
    'Mean excess returns',
    'sd',
    'min',
    'max',
    'Mean forward discount',
    'sd',
    'min',
    'max',
    'Mean spread',
    'sd',
    'min',
    'max'
  )

  output
}


IndividualReturns = function(fp, hp, rx) {
  # Calculates statistics on excess returns from trading momentum in individual
  # currencies; go long if the ccy had a had a positive mean return during the
  # formation period, otherwise short.

  # Arguments:
  # "fp" formation period in months (integer).
  # "hp" holding period in months (integer).
  # "rx" monthly log excess returns per currency (xts).

  # Returns: numeric matrix, one column per ccy and following rows
  # "$mean" mean monthly log excess return of the trading (numeric).
  # "$sd" standard deviation of monthly returns (numeric).
  # "$skew" skew of monthly returns (numeric).
  # "$kurtosis" kurtosis of monthly returns (numeric).
  # "$sharpe" annualised sharpe ratio of monthly returns (numeric).

  x = ifelse(
    HoldPositions(filter(rx, rep(1 / fp, fp), sides=1), hp) > 0,
    lag(rx, -1),   # Long (positive formation period mean return).
    lag(-rx, -1)   # Short.
  )

  colnames(x) = colnames(rx)
  x = as.xts(data.frame(1200 * x), order.by=as.Date(index(rx)))  # Annualised %

  # Compute and return statistics for results per ccy.
  apply(x, 2, function(ccy) {ccy = na.omit(ccy)
    c(mean     = mean(ccy),
      sd       = sd(ccy) / sqrt(12),
      skew     = skewness(ccy),
      kurtosis = kurtosis(ccy),
      sharpe   = mean(ccy) / sd(ccy) / sqrt(12)
  )})
}


LongTermReturns = function(fp, hp, fstat, vstat, fees, ports, dat) {
  # Computes mean annualised returns of momentum strategies by number of months
  # into a certain holding period. New long and short portfolios are formed every
  # period, and held for a longer term (overlapping holding periods). 

  # Arguments:
  # "fp" formation period in months (integer)
  # "hp" holding period in months, 60 in default analysis (integer)
  # "fstat" name of the statistic used to form portfolios (character)
  # "vstat" name of the statistic used to compute returns (character)
  # "fees" the effective spread used to estimate fees (numeric)
  # "ports" number of portfolios to split the currencies into (integer)

  # Returns:
  # "$all.results" a matrix with "hp" rows, each column representing the returns
  #   of a strategy by number of periods into the holding period.
  # "$mean.results" a numeric vector length "hp" with mean cumulative monthly
  #   return by number of periods into the holding period.

  # Trim rows consisting entirely of NAs.
  dat = lapply(dat, "[", rowSums(is.finite(dat[[fstat]])) > 0, )
  dat = lapply(dat, "[", rowSums(is.finite(dat[[vstat]])) > 0, )
#  fstat = fstat[rowSums(is.finite(fstat)) > 0, ]
#  vstat = vstat[rowSums(is.finite(vstat)) > 0, ]

  results = sapply(1:hp, function(start.row) {

    # Get trading result for each offset up to "hp" periods.

    dat[[fstat]] = last(dat[[fstat]], n=nrow(dat[[fstat]]) + 1 - start.row)
    dat[[vstat]] = last(dat[[vstat]], n=nrow(dat[[vstat]]) + 1 - start.row)

    x = MomResults(fees=fees,
      TradeMom(fp, hp, fstat, vstat, ports, dat)
    )$mag.returns

    x = na.omit(x)

    # Split the results into chuncks of "hp" periods
    x = split(x, ceiling(seq_along(x) / hp))

    x = lapply(x, as.numeric)
    x = lapply(x, function(y) c(y, rep(NA, hp - length(y))))
    Reduce(cbind, x)
  })

  all.results = Reduce(cbind, results)
  mean.results = cumsum(rowMeans(all.results, na.rm=TRUE)) / (1:hp)

  list(all.results=all.results, mean.results=mean.results)
}


MovingAvgReturns = function(results, ma.periods) {
  # Computes moving average log returns from monthly log returns.

  # Arguments: "results" periodic return data (xts)
  # "ma.months" number of months for computing average (integer)

  # Returns: moving average returns (xts)

  x = filter(results, rep(1 / ma.periods, ma.periods), sides=1)
  na.omit(as.xts(as.numeric(x), order.by=index(results)))
}


PortfolioBelongings = function(allocs) {
  # Calculates the percentage of a) total trading months and b) available months for a
  # currency, that a currency is part of the long and short portfolios.

  # Arguments: "allocs" monthly portfolio allocations (xts).

  # Returns: a list ($ccy.time and $total.time) of lists ($long and $short) containing
  #   the percentages per currency (named numeric vector).

  ccy.time = lapply(1:max(allocs, na.rm=TRUE), function(i) {
    colMeans(allocs == i, na.rm=TRUE)
  })

  names(ccy.time) = as.character(1:max(allocs, na.rm=TRUE))
  names(ccy.time)[[1]] = paste(names(ccy.time)[[1]], " (short)")
  names(ccy.time)[[max(allocs, na.rm=TRUE)]] =
    paste(names(ccy.time)[[max(allocs, na.rm=TRUE)]], " (long)")

  allocs = ifelse(is.finite(allocs), allocs, 0)   # NAs to zero for averaging.

  total.time = list(   # Percentage of total trading time.
    long = colMeans(allocs == max(allocs)),
    short = colMeans(allocs == 1)
  )

  list(ccy.time = ccy.time, total.time = total.time)
}


RecursiveMomReturns = function(fstat, vstat, fees, ports, lags=1:120,
  no.secondary.portfolios=9) {
  # Calculates the excess returns of recursive momentum strategies. The
  # historical excess returns of 144 different momentum strategy variations
  # (1-12 months formation and holding periods) form the basis of splitting
  # these strategies into different portfolios (by default 9), and every month
  # we go long the "best" of these and short the "worst". Excess returns are
  # considered over a period of 1-120 months in forming the portfolios.

  # Arguments:
  # "fstat" name of the statistic used to form portfolios (character)
  # "vstat" name of the statistic used to compute returns (character)
  # "dat" list of xts time series with data on:
  #   "$spot" spot prices (xts)
  #   "$spot.bid" spot bid prices (xts)
  #   "$spot.ask" spot ask prices (xts)
  #   "$forward.bid" forward bid prices (xts)
  #   "$forward.ask" forward ask prices (xts)
  # "ports" number of portfolios to form (integer)
  # "lags" lags in computing historical returns to consider (integer vector)

  # Returns: list with one entry per lag each containing a matrix:
  #   rows - "results" mean annualised excess returns, "sr" sharpe ratio,
  #   "se" standard error, and one column per portfolio.

  # Get the results from first-order momentum strategies.
  mom = lapply(1:12, function(fp) {
    lapply(1:12, function(hp) {
      memoizedCall(dirs=c(project.tag, "MomResults"), MomResults, fees=fees,
        memoizedCall(dirs=c(project.tag, "MomResults"), TradeMom, fp, hp, fstat, vstat, ports, dat)
      )$mag.returns
    })
  })

  results = Reduce(cbind, Reduce(cbind, mom))

  items = expand.grid(1:12, 1:12)
  colnames(results) = sapply(1:nrow(items), function(x) paste0(items[x,], collapse=","))

  # Compute moving average mom-strat restults in order to rank strategies.
  lagged.ma.results = results

  op = lapply(lags, function(lag.months) {
    lagged.ma.results[] = filter(results, rep(1 / lag.months, lag.months), sides=1)
    allocations = results
    allocations[] =
      t(apply(apply(lagged.ma.results, 1, rank) - 1, 1:2, "%/%", 16) + 1)
    # Calculate the mean monthly returns of each portfolio.
    x = sapply(1:9, function(i) {
      rowMeans(na.rm=TRUE,
        ifelse(data.frame(lag(allocations)) == i, 1, NA)
        *
        results[]
      )
    })

    x = cbind (x, (x[ , 9] - x[ , 1]))
    colnames(x) = c(as.character(1:9), "longshort")

    x = list(
      results = apply(x, 2, mean, na.rm=TRUE) * 12 * 100,
      sr = apply(x, 2, function(y){
        mean(y, na.rm=TRUE) / sd(y, na.rm=TRUE)
      }),
      se = apply(x, 2, BootstrapT)
    )
  })

  # Collate into one data frame and return.
  op = lapply(lapply(op, Reduce, f=rbind), function(x) {
    rownames(x) = c("results", "sr", "se");x
  }) 

  names(op) = as.character(lags)
  op
}


SubsetReturns = function(fp, hp, fstat, vstat, fees,  ports, dat,
  sample.size, runs, verbose=c(FALSE, TRUE)) {
  # Computes mean annualised excess returns of momentum strategies with a
  # limited number of currencies available. Currencies are sampled
  # randomly within the constraint repeatedly, and a mean result is reported.
 
  # Arguments:
  # "fp" formation period in months (integer)
  # "hp" holding period in months, 60 in default analysis (integer)
  # "fstat" name of the statistic used to form portfolios (character)
  # "vstat" name of the statistic used to compute returns (character)
  # "fees" the effective spread used to estimate fees (numeric)
  # "ports" number of portfolios to split the currencies into (integer)
  # "sample.size" number of currencies to limit the trading to (integer)
  # "runs" number of times to re-sample (integer)
  # "verbose" whether to print progress to screen (boolean)

  # Returns: a list of lists with one item representing the mean annualised excess
  # return per formation period / holding period combination.

  # Draw "no.ccies" randomly "no.simulations" times.
  subsets = replicate(runs, sample(colnames(dat$spot), sample.size))

  apply(subsets, 2, function(subset) {

    if(verbose[[1]]) {print(paste("Processing sample size:", length(subset)))}

    x = MomResults(fees=fees,
      TradeMom(fp, hp, fstat, vstat, ports, lapply(dat, "[", , subset))
    )

    mean(x$mag.returns, na.rm=TRUE)
  })
}
