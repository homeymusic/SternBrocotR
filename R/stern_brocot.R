#' Approximate a real number as a fraction using the Stern-Brocot tree
#'
#' @param x A real number to approximate as a fraction.
#' @param uncertainty Either a single numeric value representing symmetrical uncertainty
#'        or a vector of two values representing lower and upper uncertainty bounds.
#' @return A dataframe with an integer, `num` (numerator) and a natural number
#'         greater than 0, `den` (denominator), representing the fraction
#'         approximation of the input.
#' @examples
#' stern_brocot(0.49, uncertainty = 0.03) # Returns 1/2
#' stern_brocot(0.49, uncertainty = c(0.04, 0.02)) # Returns 1/2
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
