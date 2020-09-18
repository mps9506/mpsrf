#' Calculate Geometric Mean
#'
#' Calculates the geometric mean of x and, optionally, the confidence interval.
#'
#' @param x a vector of numeric values
#' @param ci numeric value for the confidence interval. Defaults to NA.
#'   Confidence intervals are calculated using bootstrap percentile intervals.
#' @param R number of bootstrap resamples, defaults to 1000.
#'
#' @return A numeric value, the geometric mean of x. Or a named list with
#'   geometric mean and the confidence intervals.
#' @importFrom boot boot boot.ci
#' @export
#'
#' @details \code{x} must be a vector of positive numeric values. The geometric
#'   mean is calculated as:
#'
#'   \loadmathjax
#'
#'   \mjsdeqn{\bar{x} = \exp  \left\[\frac{1}{n}\sum_{i=1}^{n}\log(x_i)\right] = e^\bar{y} }
#'
#'
#'
#'   where:
#'
#'   \mjsdeqn{\bar{y} = \frac{1}{n} \sum_{i=1}^n y_i}
#'
#'   \mjsdeqn{y_i = log(x_i), \;\; i = 1, 2, \ldots, n}
#'
#'   or more simply, the geometric mean of \code{x} is equal to the antilog of the mean of log(\code{x}).
#'
#'
#' @examples x <- rlnorm(1000, log(100), log(3))
#' calc_geomean(x)
#' calc_geomean(x, ci = .95, R = 1000)


calc_geomean <- function(x, ci = NA, R = 1000) {

  ## check that all values in x are > 0
  if (any(x <= 0)) {
    stop("Non-Positive values in x")
  }

  ## check that x is numeric
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  ## calculate and return geometric mean
  ## as a single numeric value
  if (is.na(ci)) {
    exp(mean(log(x)))
  }

  ## if confidence interval is needed
  ## return named list with
  ## geometric mean plus interval
  else {
   lapply(mean_ci(log(x), ci, R),
          exp)

  }
}


#' Calculate Mean and Confidence Intervals
#'
#' Calculates the bootstrap percentile confidence intervals around the mean
#'
#' @param x numeric vector
#' @param ci desired confidence interval
#' @param R number of resamples
#'
#' @return named list with mean and confidence interval
#' @export
#' @keywords internal
#' @importFrom stats var
mean_ci <- function(x, ci, R = 1000) {
  boot.mean <- boot(x,
                    function(x, i) {
                      m <- mean(x[i], na.rm = FALSE)
                      n <- length(i)
                      v <- (n-1) *var(x[i]) / n^2
                      c(m, v)
                    },
                    R = R)

  ci <- boot.ci(boot.mean, conf = ci, type = "perc")

  return(list(mean = boot.mean$t0[1],
              lwr.ci = ci[[4]][[4]],
              upr.ci = ci[[4]][[5]]))
}
