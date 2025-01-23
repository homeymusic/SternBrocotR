#' @name stern_brocot_cpp
#' @title Stern-Brocot Approximation
#'
#' @description Approximates a floating-point number to a rational fraction within a specified uncertainty
#' using the Stern-Brocot tree. This function is implemented in C++ for performance.
#'
#' @param x Numeric. A number to approximate as a rational fraction.
#' @param valid_min Numeric. The lower bound of the acceptable range for the approximation.
#' @param valid_max Numeric. The upper bound of the acceptable range for the approximation.
#'
#' @return A data frame with the following columns:
#'   - `x`: The input value to approximate.
#'   - `num`: The numerator of the fraction.
#'   - `den`: The denominator of the fraction.
#'   - `approximation`: The approximate value of the fraction (`num / den`).
#'   - `error`: The difference between `approximation` and `x`.
#'   - `valid_min`: The lower bound of the uncertainty range.
#'   - `valid_max`: The upper bound of the uncertainty range.
#'   - `depth`: The depth of the Stern-Brocot tree traversal.
#'   - `path`: A binary string representing the path taken in the Stern-Brocot tree.
#'   - `path_id`: The binary path converted to an integer.
#'
#' @examples
#' \dontrun{
#'   stern_brocot_cpp(0.49, 0.48, 0.50)
#' }
#'
#' @export
NULL
