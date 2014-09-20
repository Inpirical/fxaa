# Core trading functions for "Long-Term FX Momentum (FXAA)":

#   "TradeMom": monthly performance of momentum strategy. 
#   "FormPortfolios": forms portfolios for trading given past performance.
#   "HoldPositions": carried positions forward during holding periods.
#   "PositionsTerminated": establishes when positions are new, carried forward,
#     terminated and already established.
#   "SplitPortfolios": splits assets discretely into portfolios given ranking.
#   "MomAllocate": gen monthly allocations given fp, hp and form. statistic.

TradeMom = function(fp, hp, fstat, vstat, ports, dat) {
  # Calculates the monthly allocations and performance results of a momentum
  # strategy; including results on all portfolios formed as well as for the
  # long-short momentum strategy.

  # Arguments:
  # "fp" formation period in months (integer)
  # "hp" holding period in months (integer)
  # "fstat" name of the statistic used to form portfolios (character)
  # "vstat" name of the statistic used to compute returns (character)
  # "dat" list of xts time series with data on:
  #   "$spot" spot prices (xts)
  #   "$spot.bid" spot bid prices (xts)
  #   "$spot.ask" spot ask prices (xts)
  #   "$forward.bid" forward bid prices (xts)
  #   "$forward.ask" forward ask prices (xts)

  # Returns: list with following entries
  # "allocs" allocation of every currency avaible to a portfolio (xts)
  # "all" matrix with column for each portfolio and "long" and "short" rows
  #    each entry a time series of the monthly returns of the portfolio (xts)
  # "longshort" the monthly returns of a long-short momentum strategy (xts)
  # "cum.longshort" cumulative returns of a long-short momentum strategy (xts)
  # "sharpe.ratio" the annualised Sharpe ratio of long-short returns (numeric)
  # "t.neweywest" NeweyWest heteroskedasticity and auto-correlation consistent
  #   t-statistic (numeric)


  # Calculate the portfolio allocations over time.

  allocs = MomAllocate(fp, hp, dat[[fstat]], ports)

  td = PositionsTerminated(allocs)

  # Calculate the returns for all the positions in our allocation.
  returns = list(
    gross     = dat[[vstat]],
    net.long  = (td * dat$rx.long.term  + (!td) * dat$rx.long.nonterm),
    net.short = (td * dat$rx.short.term + (!td) * dat$rx.short.nonterm)
  )

  colnames(allocs) = colnames(dat[[vstat]])

  all.rets = sapply(1:ports, function(portfolio) {
    allocs = (allocs == portfolio)
    allocs[allocs == FALSE] = NA
    lagged.allocs = lag(allocs)
    lapply(returns, '*', lagged.allocs)
  })


  # Calculate gross and net returns of a long-short strategy.

  cbind(
    gross = as.xts(order.by=index(dat[[vstat]]),
        rowMeans(na.rm=TRUE, all.rets[["gross", ncol(all.rets)]])
      - rowMeans(na.rm=TRUE, all.rets[["gross", 1]])
    ),

    net = as.xts(order.by=index(dat[[vstat]]),
        rowMeans(na.rm=TRUE, all.rets[["net.long", ncol(all.rets)]])
      - rowMeans(na.rm=TRUE, all.rets[["net.short", 1]])
    )
  )
}


MomResults = function(returns, fees) {
  # Calculates the statistics of a long-short strategy given gross and net
  # returns and a fee factor.

  # Arguments:
  # "returns" time series (xts) with following columns:
  #   "gross" gross monthly log returns of a long-short strategy
  #   "net" net monthly log returns of a long-short strategy (xts).
  # "fees" assumption about effective bid-ask spread to estimate fees (e.g.
  #  0 implies no fees, 1 implies full bid-ask spread, and 2 implies double
  #  the bid-ask spreads (numeric).

  # Returns a list with following entries:
  # "$mag.returns" the monthly returns of a long-short momentum strategy (xts)
  # "$cum.returns" cumulative returns of a long-short momentum strategy (xts)
  # "$sharpe" the annualised Sharpe ratio of long-short returns (numeric)
  # "$t.nw" NeweyWest heteroskedasticity and auto-correlation consistent
  #   t-statistic (numeric)
  # "$t.boot" fixed block bootstrap of t statistic of Sharpe ratio (numeric)

  # Apply fees by taking weighted average of gross and net returns.
  r = ((fees * returns$net) + ((1 - fees) * returns$gross))

  # Compute mean monthly return.
  mean.r = mean(r, na.rm=TRUE)

  list(
    mag.returns	= r,
    cum.returns	= cumsum(na.omit(r)),
    sharpe	= sqrt(12) * (mean.r / sd(r, na.rm=TRUE)),

    t.nw	= tryCatch(
      (mean.r / sqrt(NeweyWest(lm(as.numeric(r) ~ 1))))[[1]],
      errror = function(e) {NA}),

    t.boot	= tryCatch(BootstrapT(r), error = function(e) {NA})
  ) 
}


FormPortfolios = function(fp, ports, stats) {
  # Forms the portfolio allocations of a momentum trading strategy.

  # Arguments:
  # "fp" formation period in months (integer).
  # "stats" time series with the values used for ranking the assets (xts).
  # "ports" number of portfolios to split the assets into (integer).

  # Returns: time series with integers indicating asset allocations (xts).

  # Calculate a weighted average statistic looking "fp" periods back.
  x = filter(stats, rep(1 / fp, fp), sides=1)

  # Rank the currencies and allocate them to the different portfolios.
  splits = sapply(1:ncol(stats), SplitPortfolios, ports=ports)

  x = t(apply(x, 1, function(i) {
    ifelse(is.na(i), NA, splits[[sum(is.finite(i))]][rank(i)])
  }))

  colnames(x) = colnames(stats)

  as.xts(x, order.by=index(stats))
}


HoldPositions = function(positions, hp) {
  # Converts a set of monthly positions to re-form only every "hp" periods
  # (holding period) — that is the portfolio formation is non-overlapping.

  # Arguments:
  # "positions" — an xts time series with an allocation of every currency
  #   to a portfolio (represented by an integer) per period.
  # "hp" — the required holding period in months (integer)

  # Returns:
  # an xts time series representing positions which are re-formed only every
  # "hp" periods.

  # Calculate the row index when trading starts.
  start.row = match(FALSE, !rowSums(positions, na.rm=TRUE))

  # Get the row index containing the positions to be held for each trading
  # period; using modular arithmetic to carry positions forward for "hp"
  # periods before re-forming.
  row.indices = c(
    # Use first row before trading has started.
    rep(1, start.row - 1), 
    # thereafter re-form every "hp" periods.
    (((1:nrow(positions) - 1)) %/% hp) * hp + start.row
  )

  # Apply the new "row.indices" to the positions.
  positions[] = positions[row.indices[1:nrow(positions)], ]
  positions
}


PositionsTerminated = function(positions) {
  # Checks whether positions are terminated at the end of every period.

  # Arguments:
  # "positions" — an xts time series with an allocation of every currency
  #   to a portfolio (represented by an integer) per period.
   
  # Returns:
  # an xts time series with returning TRUE for every allocation that is
  # terminating, FALSE for those not terminating and NA for those not defined.

  # Terminating — either the allocation is different in the following period:
  lag(diff(positions), -1) |
    # or, it is defined in the current period but NA in the following period:
    (is.finite(positions) & is.na(lag(positions, -1)))
}


SplitPortfolios = function(no.assets, ports) {
  # Discretely splits N ordered assets into M portfolios. Borderline assets are
  # allocated i.f.f. >= 50% weight would be allocated on a continuous basis.

  # Arguments:
  # "no.assets" number of assets to split into portfolios (integer).
  # "ports" number of portfolios into which to split the assets (integer).
 
  # Returns: vector length equal "no.assets" with allocations (integer).

  1 + apply(MARGIN=2, FUN=sum, X=sapply(1:no.assets / no.assets,
      "-",
      (1:ports) / ports
    ) > (1 / (2 * no.assets))
  )
}


MomAllocate = function(fp, hp, fstat, ports) {
  # Calculates monthly portfolio allocations for a momentum strategy.

  # Arguments:
  # "fp"	formation period in months (integer)
  # "hp"	holding period in months (integer)
  # "fstat"	time series of statistic used to form portfolios (xts)
  # "ports"	number of portfolios to form (integer)

  # Returns: time series with allocation of every currency to a portfolio (xts)

  HoldPositions(hp=hp,
    last(memoizedCall(dirs=c(project.tag, "FormPortfolios"), FormPortfolios, fp, ports, fstat), nrow(fstat))
  )
}
