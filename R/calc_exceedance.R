#' Calculate Exceedance Probability
#'
#' Calculates the exceedance probability, or the flow-duration percentile.
#' @param x a vector of values. Missing values are left as is. If you want to
#'   treat as zero, replace with 0. if you want to treat them as the smallest
#'   value, replace with -Inf before applying the function.
#' @param ties How to handle ties in ranked values. One of \code{c("average",
#'   "first", "last", " random", "max", "min")}. Defaults to \code{"min"}.
#'
#' @return A vector of length x of the exceedance probability for each value of
#'   x.
#' @export
#' @details \code{x} must be a vector of positive numeric values. The exceedance probability is calculated as:
#'
#'   \loadmathjax
#'   \mjsdeqn{P = \frac{m}{n+1}}
#'
#'   where
#'
#'   \mjseqn{P} is the exceedance probability,
#'
#'   \mjseqn{m} is the descending rank value,
#'
#'   \mjseqn{n} is the total number of values
#'
#' @references Searcy, J. C. (1959). Manual of hydrology, 2, Low flow techniques, flow duration curves. US Geol. Surv. Water Supply Pap, 1542.
#'
#' @examples x <- seq(1:100)
#' calc_exceedance(x)
#'
calc_exceedance <- function(x, ties = "min") {

  ## check that x is numeric
  if (!is.numeric(x)) {
    stop("x must be a numeric vector")
  }

  n <- length(x)
  m <- rank(-x, ties.method = ties, na.last = "keep")

  p <- m/(n+1)
  return(p)
}
