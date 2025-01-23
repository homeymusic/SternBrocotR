#' Approximate a real number as a fraction using the Stern-Brocot tree
#'
#' This function approximates a real number as a rational fraction (numerator and denominator)
#' using the Stern-Brocot tree. It supports specifying a tolerance (uncertainty) to determine
#' how close the approximation should be to the real number.
#'
#' The method is inspired by the algorithms described in:
#'
#' - Forišek, M. (2007). Approximating rational numbers by fractions. In International
#'   Conference on Fun with Algorithms (pp. 156-165). Springer Berlin Heidelberg.
#' - Stolzenburg, F. (2015). Harmony perception by periodicity detection. Journal of
#'   Mathematics and Music, 9(3), 215-238.
#'
#' @param x A single numeric value to approximate as a fraction. Must be a finite value.
#' @param uncertainty Either:
#'   - A single positive numeric value representing symmetrical uncertainty bounds (i.e., ±uncertainty), or
#'   - A vector of two positive numeric values, where the first element is the lower uncertainty and the second
#'     is the upper uncertainty, defining the range `[x - lower, x + upper]`.
#'
#' @return A data frame with the following columns:
#'   - `x`: The original value of `x`.
#'   - `num`: The numerator of the approximated fraction (an integer).
#'   - `den`: The denominator of the approximated fraction (a natural number > 0).
#'   - `approximation`: The value of the fraction `num / den`.
#'   - `error`: The difference between the approximation and the original value of `x`.
#'   - `valid_min`: The lower bound of the uncertainty range.
#'   - `valid_max`: The upper bound of the uncertainty range.
#'   - `depth`: The depth of the Stern-Brocot tree traversal (number of steps taken).
#'   - `path`: The path taken in the Stern-Brocot tree as a binary string.
#'   - `path_id`: The binary path represented as an integer.
#'
#' @examples
#' # Approximation with symmetrical uncertainty
#' stern_brocot(0.49, uncertainty = 0.03)  # Returns approximately 1/2
#'
#' # Approximation with asymmetrical uncertainty
#' stern_brocot(0.49, uncertainty = c(0.04, 0.02))  # Returns approximately 1/2
stern_brocot <- function(x, uncertainty) {
  # Check that `x` is numeric and length 1
  if (!is.numeric(x) || length(x) != 1) {
    stop("`x` must be a single numeric value.")
  }

  # Validate `uncertainty`
  if (!is.numeric(uncertainty)) {
    stop("`uncertainty` must be numeric")
  }

  # Handle symmetrical uncertainty
  if (length(uncertainty) == 1) {
    lower_uncertainty <- uncertainty
    upper_uncertainty <- uncertainty
  } else if (length(uncertainty) == 2) {
    lower_uncertainty <- uncertainty[1]
    upper_uncertainty <- uncertainty[2]
  } else {
    stop("`uncertainty` must be a single value or a vector of two values (lower and upper bounds).")
  }

  # Ensure uncertainties are non-negative
  if (lower_uncertainty < 0 || upper_uncertainty < 0) {
    stop("Uncertainty values must be non-negative.")
  }

  stern_brocot_cpp(
    x,
    valid_min = x - lower_uncertainty,
    valid_max = x + upper_uncertainty
  )

}
