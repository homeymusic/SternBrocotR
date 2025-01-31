% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/stern_brocot_cpp.R
\name{stern_brocot_cpp}
\alias{stern_brocot_cpp}
\title{Stern-Brocot Approximation}
\arguments{
\item{x}{Numeric. A number to approximate as a rational fraction.}

\item{valid_min}{Numeric. The lower bound of the acceptable range for the approximation.}

\item{valid_max}{Numeric. The upper bound of the acceptable range for the approximation.}
}
\value{
A data frame with the following columns:
\itemize{
\item \code{x}: The input value to approximate.
\item \code{num}: The numerator of the fraction.
\item \code{den}: The denominator of the fraction.
\item \code{approximation}: The approximate value of the fraction (\code{num / den}).
\item \code{error}: The difference between \code{approximation} and \code{x}.
\item \code{valid_min}: The lower bound of the uncertainty range.
\item \code{valid_max}: The upper bound of the uncertainty range.
\item \code{depth}: The depth of the Stern-Brocot tree traversal.
\item \code{path}: A binary string representing the path taken in the Stern-Brocot tree.
\item \code{path_id}: The binary path converted to an integer.
}
}
\description{
Approximates a floating-point number to a rational fraction within a specified uncertainty
using the Stern-Brocot tree. This function is implemented in C++ for performance.
}
\examples{
\dontrun{
  stern_brocot_cpp(0.49, 0.48, 0.50)
}

}
