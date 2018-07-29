generate_data <- function(n, p) {
  return(list(covariates = matrix(rnorm(n * p), n, p),
              responses = rnorm(n)))
}

model_select <- function(covariates, responses, cutoff) {
  model <- lm(responses ~ covariates)
  retained <- summary(model)$coefficients[-1,4] <= cutoff
  
  if (!any(retained)) { # stop early if no significant p-values
    return(c())
  }
  
  model <- lm(responses ~ covariates[, retained])
  return(as.vector(summary(model)$coefficients[-1, 4]))
}

run_simulation <- function(n_trials, n, p, cutoff, datapath,
                           method = model_select) {
  replicates <- replicate(n_trials, {
    data <- generate_data(n, p)
    return(method(data$covariates, data$responses, cutoff))
  })
  
  p_values <- do.call(c, replicates)
  save(p_values, file = datapath)
}