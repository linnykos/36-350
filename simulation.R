generate_data <- function(n, p) {
  return(list(covariates = matrix(rnorm(n * p), n, p),
              responses = rnorm(n)))
}