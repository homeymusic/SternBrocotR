#' Approximate a real number as a coprime rational fraction using the Stern-Brocot tree
#'
#' This function approximates a real number as a coprime rational fraction
#' using the Stern-Brocot tree. It supports specifying an uncertainty to determine
#' how close the approximation should be to the real number.
#'
#' The method is inspired by the algorithms described in:
#'
#' 1. Stern, M. (1858). Ueber eine zahlentheoretische Funktion. *Journal für die reine und angewandte Mathematik, 55*, 193–220.
#' 2. Brocot, A. (1862). Calcul des rouages par approximation: Nouvelle méthode. *A. Brocot.*
#' 3. Graham, R. L., Knuth, D. E., & Patashnik, O. (1994). *Concrete mathematics* (2nd ed., pp. 115–123). Addison-Wesley.
#' 4. Forišek, M. (2007). Approximating rational numbers by fractions. In *International Conference on Fun with Algorithms* (pp. 156–165). Berlin, Heidelberg: Springer Berlin Heidelberg.
#' 5. Stolzenburg, F. (2015). Harmony perception by periodicity detection. *Journal of Mathematics and Music, 9*(3), 215–238.
#'
#' @param x A single numeric value to approximate as a fraction.
#' @param lower_uncertainty The lower uncertainty
#' @param upper_uncertainty The upper uncertainty
#'
#' @return A data frame with the following columns:
#'   - `num`: The numerator of the approximated fraction (an integer).
#'   - `den`: The denominator of the approximated fraction (a natural number > 0).
#'   - `approximation`: The value of the fraction `num / den`.
#'   - `error`: The difference between the approximation and the original value of `x`.
#'   - `depth`: The depth of the Stern-Brocot tree traversal.
#'   - `path`: The path taken in the Stern-Brocot tree as a binary string.
#'   - `path_id`: The binary path represented as an integer.
#'   - `x`: The original value of `x`.
#'   - `lower_uncertainty`: The lower uncertainty
#'   - `upper_uncertainty`: The upper uncertainty range.
#'   - `valid_min`: The lower bound of the uncertainty range.
#'   - `valid_max`: The upper bound of the uncertainty range.
#'
#' @examples
#' # Approximation with symmetrical uncertainty
#' stern_brocot(sqrt(2), uncertainty = 0.03)
#'
#' # Approximation with asymmetrical uncertainty
#' stern_brocot(sqrt(12), uncertainty = c(0.01, 0.02))
#'
#' @export
stern_brocot <- function(x, lower_uncertainty, upper_uncertainty) {

  stern_brocot_cpp(
    x,
    lower_uncertainty = lower_uncertainty,
    upper_uncertainty = upper_uncertainty
  )

}
