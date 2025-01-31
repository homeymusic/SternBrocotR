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
    result = (result << 1) | bits[i];  // Shift left and add current bit
  }
  return result;
}

 //' stern_brocot_cpp
 //'
 //' Approximates a floating-point number to arbitrary uncertainty.
 //'
 //' This implementation is based on the Stern-Brocot tree algorithm as described in:
 //' - Forišek M. Approximating rational numbers by fractions. In International Conference on Fun with Algorithms 2007 Jun 3 (pp. 156-165). Berlin, Heidelberg: Springer Berlin Heidelberg.
 //' - Stolzenburg F. Harmony perception by periodicity detection. Journal of Mathematics and Music. 2015 Sep 2;9(3):215-38.
 //'
 //' @param x Number to convert to rational fraction
 //' @param valid_min Lower bound
 //' @param valid_max Upper bound
 //'
 //' @return A data frame with results and metadata about the tree path.
 //' @export
 // [[Rcpp::export]]
DataFrame stern_brocot_cpp(const double x,
                           const double valid_min,
                           const double valid_max) {

  if (valid_min <= 0) {
    stop("STOP: valid_min must be greater than 0");
  }
  if (x <= valid_min) {
    stop("STOP: x must be greater than valid_min");
  }
  if (valid_max <= x) {
    stop("STOP: x must be less than valid_max");
  }

  std::vector<int> path = {1};

  int left_num    = 0, left_den    = 1;
  int mediant_num = 1, mediant_den = 1;
  int right_num   = 1, right_den   = 0;

  double approximation = 1.0;

  while ((approximation < valid_min) || (valid_max < approximation)) {
    if (approximation < valid_min) {
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
    approximation = (double) mediant_num / mediant_den;

  }

  if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

  return DataFrame::create(
    _["x"] = x,
    _["num"] = mediant_num,
    _["den"] = mediant_den,
    _["approximation"] = approximation,
    _["error"] = round_to_precision(approximation - x),
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max,
    _["depth"] = path.size(),
    _["path"] = as_string_cpp(path),
    _["path_id"] = as_integer_cpp(path)
  );
}
