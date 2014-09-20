# Core Support Vector Machine related functions for "Long-Term FX Momentum (FXAA)". 

#  "RxFdMachine": trains a primitive SVM based on returns and forward discounts
#  "RxFdMachineMap": maps the predictions by an "RxFdMachine" model based on a
#     range of returns and forward discount inputs
#  "TrainSVM": trains an SVM given features, responses and parameters using a
#     grid search.
#  "TradeSVM": wrapper function for TradeMom which uses the predicitons of a
#     SVM to form portfolios.


RxFdMachine = function(fp, hp, dat, ports, months=120, cross=2, sample.size=150) {
  # Trains a primitive support vector machine to forecast mean excess returns
  # "hp" periods ahead given information on mean excess returns and mean forward
  # discounts "fp" periods back.

  # Arguments:
  # "fp" formation period in months (integer)
  # "hp" holding period in months (integer)
  # "dat" list of xts time series with data on:
  #   "$spot" spot prices (xts)
  #   "$spot.bid" spot bid prices (xts)
  #   "$spot.ask" spot ask prices (xts)
  #   "$forward.bid" forward bid prices (xts)
  #   "$forward.ask" forward ask prices (xts)
  # "ports" number of portfolios to form (integer)
  # "months" number of months data to use for training the machine (integer)
  # "cross" number of cross-validations (integer)
  # "sample.size" number of observations to select for training (integer)

  # Returns:
  # "monthly.results" monthly excess returns of the strategy (xts).
  # "results" cumulative excess returns of the strategy (xts).
  # "allocations" allocations time series of all currencies to portfolios (xts).
  # "forecasts" forecast excess returns of currencies by sv machine (xts).
  # "machine" the trained support vector machine (svm object).
  # "features" features per currency used for forecasting (list of matrices).

  # Generate the features
  features = lapply(c(dat$rx, dat$fd), function(x) {
    filter(x, rep(1 / fp, fp), sides=1)
  })

  # Generate the responses.
  responses = first(n=months, dat$rx)
  responses[] = filter(responses, rep(1 / hp, hp), sides=1)
  responses = lag(responses, k=(-fp))

  # Create one matrix of responses (first column) and associated features.
  d = lapply(1:ncol(features[[1]]), function(i) {
    matrix(ncol=3, cbind(responses[ , i],
      first(n=months, features[[1]][ , i]),
      first(n=months, features[[2]][ , i])
    ))
  })

  d = Reduce(rbind, d)
  d = d[complete.cases(d), ]
  d = d[sample(1:nrow(d), sample.size), ]

  # Train the support vector machine.
  machine = TrainSVM(d[ , -1], d[ , 1], cross=cross, kernel="linear")

  # Generate forecasts using the machine.
  forecasts = lapply(1:ncol(features[[1]]), function(i) {
    apply(cbind(features[[1]][ , i], features[[2]][ , i]), 1, function(r) {
      if(!any(is.na(r))) {
        predict(machine$best.model, matrix(r, nrow=1))
      } else {
        NA
      }
    })
  })

  forecasts = Reduce(cbind, forecasts)
  colnames(forecasts) = colnames(responses)
  forecasts = as.xts(forecasts, order.by=index(dat$spot))

  # Form portfolio allocations given the forecasts.
  allocations = forecasts
  allocations[] = t(apply(forecasts, 1, function(i) {
    # Cover the special case with zero observations
    if(!any(is.finite(i))) {return(rep(NA, length(i)))}
    ifelse(is.na(i), NA, SplitPortfolios(sum(is.finite(i)), ports)[rank(i)])
  }))

  allocations = HoldPositions(
    last(allocations, n=(nrow(allocations) -months)), hp
  )

  x = data.frame(lag(allocations))
  results = last(n=(nrow(dat$spot)-months), dat$rx)

  short.results = ifelse(x == 1, -results, NA)
  long.results = ifelse(x == ports, results, NA)

  results = ifelse(is.finite(short.results), short.results,
    ifelse(is.finite(long.results), long.results, NA)
  )

  monthly.results = 2 * rowMeans(na.rm=TRUE, results)
  results = 2 * cumsum(na.omit(rowMeans(na.rm=TRUE, results)))
  # Calculate a benchmark.
  features = lapply(1:ncol(features[[1]]), function(i) {
    matrix(ncol=2, cbind(
      features[[1]][ , i],
      features[[2]][ , i]
    ))
  })

  list(
    monthly.results =
      as.xts(monthly.results, order.by=as.Date(names(monthly.results))),
    results = as.xts(results, order.by=as.Date(names(results))),
    allocations = allocations,
    forecasts = forecasts,
    machine = machine,
    features = features
  )
}


RxFdMachineMap = function(machine, rx.range, fd.range, no.values=25) {
  # Creates predictions of a support vector machine with two features
  # (excess returns and forward discounts) given a value range for each
  # feature.

  # Arguments:
  # "machine" a two-feature support vector machine model (svm$best.model)
  # "rx.range" mimimun and maximum value of excess return feature (numeric)
  # "fd.range" mimimun and maximum value of forward dicount feature (numeric)

  # Returns (data frame):
  # "$rx" the excess return value used in a forecast (numeric).
  # "$fd" the forward discounts value used in a forecast (numeric).
  # "$value" the forecast by the svm given "rx" and "fd" (numeric).

  rx.values = seq(rx.range[[1]], rx.range[[2]], length.out=no.values)
  fd.values = seq(fd.range[[1]], fd.range[[2]], length.out=no.values)

  inputs = expand.grid(rx.values, fd.values)
  outputs = apply(inputs, 1, function(i) {
    predict(machine, matrix(i, nrow=1))
  })

  op = cbind(inputs, outputs)
  colnames(op) = c("rx", "fd", "value")
  op
}


TrainSVM = function(xx, yy, kernel, gamma=10^(-5:-1), cost=10^(0:2),
  sampling="cross", method=c("all", "prune", "add"), cross=10) {
  # Train a support vector machine using either all available features, or
  # an additive or subtractive method to select the best features. This uses a
  # parameter grid search to find the "best" parameters.

  # Arguments:
  # "xx" features data, one feature per column (matrix)
  # "yy" response data, one per features row (numeric vector)
  # "gamma" gamma values to consider in grid search (numeric vector)
  # "cost" cost parameters to consider in grid search (numeric vector)
  # "sampling" the sampling method to use (default "cross" for cross-validation
  #   (character)
  # "method" how features should be selected, one of "all": all features are
  #   used to train the SVM, "add" features are added one at a time, each time
  #   adding the one which leads to the greatest marginal performance or 
  #   "prune": removing features one by one until there is no further improvement,
  #   each time removing for greatest marginal performance gain  (character).
  # "cross" the number of cross-validations (integer)

  # Returns:
  # A trained support vector machine (e1071 svm object)

  # Function to tune an svm given features matrix.
  tune.svm = function(x) {
    tune(svm, train.x=x, train.y=yy, kernel=kernel,
      ranges=list(gamma=gamma, cost=cost),
      tunecontrol=tune.control(sampling=sampling, cross=cross)
    )
  }

  if(method[1] == "add") {
    best.svm = NULL
    best.perf = 1e9

    # Maintained sorted, the content are the column indexes in the original matrix
    features = c()
    features.available = seq(1, ncol(xx))

    repeat {   # Add features until no marginal improvement in performance.
      candidate = 0L
      for(feature in 1:length(features.available)) {
        # Get the matrix for the current tunning and tune
        zz = xx[ , sort(c(features, features.available[feature]))]
        new.svm = tune.svm(zz)
        # Check the performance improvement
        new.perf = round(new.svm$best.performance, 8)

        if(new.perf < best.perf) {
           best.svm = new.svm
           best.perf = new.perf
           candidate = feature 
        }
      }

      if(!candidate | !length(features.available)) {break}   # Break; done! 
      # Update with improvement:
      features = sort(c(features, features.available[candidate]))
      features.available = features.available[-candidate]
     }
   } else {
      # Train the SVM
      best.svm = tune.svm(xx)

      # Keep track of the original features (by column-index)
      features = seq(1, ncol(xx))

      # Optimise feature selection using pruning.
      if(method[1] == "prune") {
        repeat {
          candidate = 0L

          for(feature in 1:ncol(xx)) {  # Tune with pruned features.
            zz = xx[  , -feature]
            new.svm = tune.svm(zz)
            if(round( new.svm$best.performance, 6 ) <
              round(best.svm$best.performance, 6)) {
              best.svm = new.svm
              candidate = feature 
            }
          }

          if(!candidate | length(features) == 1) {break}   # Break; done!
          xx = xx[ , -candidate]
          features = features[-candidate]
        }
      }
    }
  best.svm
}


TradeSVM = function(machine, fp, hp, fees, ports, dat, timer="") {
  # Wrapper function for TradeMom, which uses a support vector machine as the portfolio
  # formation engine.

  # Arguments:
  # "timer" xts time index for subsetting the data (character)

  # Returns: a "TradeMom" object created by using the SVM for forecasting.

  rx = filter(dat$rx, rep(1 / fp, fp), sides=1)
  fd = filter(dat$fd, rep(1 / fp, fp), sides=1)

  # Generate predictions using the machine.
  predictions = lapply(1:ncol(rx), function(i) {
    apply(cbind(rx[ , i], fd[ , i]), 1, function(r) {
      if(!any(is.na(r))) {
        predict(machine, matrix(r, nrow=1))
      } else {
        NA
      }
    })
  })

  predictions = Reduce(cbind, predictions)
  colnames(predictions) = colnames(dat$spot)
  predictions = as.xts(predictions, order.by=index(dat$spot))

  dat$pred = predictions

  memoizedCall(MomResults, fees=fees,
    memoizedCall(TradeMom, 1, hp, "pred", "rx", ports, dat)[timer]
  )
}
