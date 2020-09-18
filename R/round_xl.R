#' Inelegant Excel Rounding
#'
#' This function mimics the rounding found in Excel where 0.5 is always round
#' away from zero. It is generally recommended to avoid using this, but
#' sometimes we have to mimic calculations done inside an Excel workbook.
#' @param x a numeric value
#' @param digits a numeric value indicating the desired digits of precision
#'
#' @return a rounded numeric value
#' @export
#' @seealso [round()] for base rounding and details of rounding at 5.
#'
#' @examples x <- .5 + -2:4
#' # using base round function
#' # expected values are: -2 0 0 2 2 4 4
#' round(x)
#'
#' # using Excel rounding
#' # the expected values are: -2 -1  1  2  3  4  5
#' round_xl(x)
round_xl <- function(x, digits = 0) {

  ## x must be numeric
  if (!is.numeric(x)) stop("x must be a numeric argument")


  x = x + abs(x) * sign(x) * .Machine$double.eps
  round(x, digits = digits)
}
