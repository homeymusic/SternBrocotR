#include <Rcpp.h>
using namespace Rcpp;

inline double round_to_precision(double value, int precision = 15) {
  double scale = std::pow(10.0, precision);
  return std::round(value * scale) / scale;
}

inline std::string as_string_cpp(const std::vector<int>& bits) {
  std::ostringstream oss;
  for (int bit : bits) {
    oss << bit;
  }
  return oss.str();
}

inline int as_integer_cpp(const std::vector<int>& bits) {
  int result = 0;
  for (size_t i = 0; i < bits.size(); ++i) {
    result = (result << 1) | bits[i]; // Shift left and add current bit
  }
  return result;
}

//' stern_brocot
//'
//' Approximates a floating-point number to arbitrary uncertainty (vectorized version).
//'
//' This implementation is based on the Stern-Brocot tree algorithm as described in:
//' - Forišek M. Approximating rational numbers by fractions. In International Conference on Fun with Algorithms 2007 Jun 3 (pp. 156-165). Berlin, Heidelberg: Springer Berlin Heidelberg.
//' - Stolzenburg F. Harmony perception by periodicity detection. Journal of Mathematics and Music. 2015 Sep 2;9(3):215-38.
//'
//' @param x Vector of numbers to convert to rational fractions.
//' @param lower_uncertainty Lower uncertainty bounds (scalar or vector).
//' @param upper_uncertainty Upper uncertainty bounds (scalar or vector).
//'
//' @return A data frame with results and metadata about the tree path.
//' @export
// [[Rcpp::export]]
DataFrame stern_brocot(const NumericVector x,
                           const NumericVector lower_uncertainty,
                           const NumericVector upper_uncertainty) {

  // Check vector lengths
  int n = x.size();
  if (lower_uncertainty.size() != 1 && lower_uncertainty.size() != n) {
    stop("lower_uncertainty must either be of length 1 or match the length of x");
  }
  if (upper_uncertainty.size() != 1 && upper_uncertainty.size() != n) {
    stop("upper_uncertainty must either be of length 1 or match the length of x");
  }

  // Expand scalar uncertainties to match the length of x
  NumericVector lower = lower_uncertainty.size() == 1 ? NumericVector(n, lower_uncertainty[0]) : lower_uncertainty;
  NumericVector upper = upper_uncertainty.size() == 1 ? NumericVector(n, upper_uncertainty[0]) : upper_uncertainty;

  // Compute valid_min and valid_max
  NumericVector valid_min = x - lower;
  NumericVector valid_max = x + upper;

  // Check validity of computed bounds
  for (int i = 0; i < n; i++) {
    if (valid_min[i] <= 0) {
      stop("STOP: valid_min must be greater than 0");
    }
    if (x[i] <= valid_min[i]) {
      stop("STOP: x must be greater than valid_min");
    }
    if (valid_max[i] <= x[i]) {
      stop("STOP: x must be less than valid_max");
    }
  }

  // Initialize result vectors
  IntegerVector nums(n), dens(n), depths(n), path_ids(n);
  NumericVector approximations(n), errors(n);
  CharacterVector paths(n);

  // Compute results for each element in x
  for (int i = 0; i < n; i++) {
    std::vector<int> path = {1};
    int left_num = 0, left_den = 1;
    int mediant_num = 1, mediant_den = 1;
    int right_num = 1, right_den = 0;

    double approximation = 1.0;

    while ((approximation < valid_min[i]) || (approximation > valid_max[i])) {
      if (approximation < valid_min[i]) {
        left_num = mediant_num;
        left_den = mediant_den;
        path.push_back(1);
      } else {
        right_num = mediant_num;
        right_den = mediant_den;
        path.push_back(0);
      }

      mediant_num = left_num + right_num;
      mediant_den = left_den + right_den;
      approximation = static_cast<double>(mediant_num) / mediant_den;
    }

    if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

    // Store results
    nums[i] = mediant_num;
    dens[i] = mediant_den;
    approximations[i] = approximation;
    errors[i] = round_to_precision(approximation - x[i]);
    depths[i] = path.size();
    paths[i] = as_string_cpp(path);
    path_ids[i] = as_integer_cpp(path);
  }

  // Return results as a DataFrame
  return DataFrame::create(
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["error"] = errors,
    _["depth"] = depths,
    _["path"] = paths,
    _["path_id"] = path_ids,
    _["x"] = x,
    _["lower_uncertainty"] = lower,
    _["upper_uncertainty"] = upper,
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max
  );
}
