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

//' stern_brocot_cpp
//'
//' Approximates a floating-point number to arbitrary uncertainty (vectorized version).
//'
//' This implementation is based on the Stern-Brocot tree algorithm as described in:
//' - Forišek M. Approximating rational numbers by fractions. In International Conference on Fun with Algorithms 2007 Jun 3 (pp. 156-165). Berlin, Heidelberg: Springer Berlin Heidelberg.
//' - Stolzenburg F. Harmony perception by periodicity detection. Journal of Mathematics and Music. 2015 Sep 2;9(3):215-38.
//'
//' @param x Vector of numbers to convert to rational fractions.
//' @param valid_min Lower bounds (scalar or vector).
//' @param valid_max Upper bounds (scalar or vector).
//'
//' @return A data frame with results and metadata about the tree path.
//' @export
// [[Rcpp::export]]
DataFrame stern_brocot_cpp(const NumericVector x,
                           const NumericVector valid_min,
                           const NumericVector valid_max) {

  // Check vector lengths
  int n = x.size();


  if (valid_min.size() != valid_max.size()) {
    stop("valid_min and valid_max must be the same length");
  }

  if ((valid_min.size() != 1 && valid_min.size() != n) ||
      (valid_max.size() != 1 && valid_max.size() != n)) {
    stop("valid_min and valid_max must either be of length 1 or match the length of x");
  }

  // Expand scalar bounds to match the length of x
  NumericVector min_vec = valid_min.size() == 1 ? NumericVector(n, valid_min[0]) : valid_min;
  NumericVector max_vec = valid_max.size() == 1 ? NumericVector(n, valid_max[0]) : valid_max;

  // Check validity of bounds
  for (int i = 0; i < n; i++) {
    if (min_vec[i] <= 0) {
      stop("STOP: valid_min must be greater than 0");
    }
    if (x[i] <= min_vec[i]) {
      stop("STOP: x must be greater than valid_min");
    }
    if (max_vec[i] <= x[i]) {
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

    while ((approximation < min_vec[i]) || (approximation > max_vec[i])) {
      if (approximation < min_vec[i]) {
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
    _["x"] = x,
    _["num"] = nums,
    _["den"] = dens,
    _["approximation"] = approximations,
    _["error"] = errors,
    _["valid_min"] = min_vec,
    _["valid_max"] = max_vec,
    _["depth"] = depths,
    _["path"] = paths,
    _["path_id"] = path_ids
  );
}
