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

// Helper function to create the DataFrame with all columns and calculated metrics
DataFrame create_stern_brocot_df(
    const double x,
    const int num,
    const int den,
    const double approximation,
    const double valid_min,
    const double valid_max,
    const std::vector<int>& path
) {

  return DataFrame::create(
    _["x"] = x,
    _["num"] = num,
    _["den"] = den,
    _["approximation"] = approximation,
    _["error"] = round_to_precision(approximation - x),
    _["valid_min"] = valid_min,
    _["valid_max"] = valid_max,
    _["depth"] = path.size(),  // Depth corresponds to the length of the path
    _["path"] = as_string_cpp(path),  // Convert path vector to string
    _["path_id"] = as_integer_cpp(path)
  );
}

 //' stern_brocot_cpp
 //'
 //' Approximates a floating-point number to arbitrary uncertainty.
 //'
 //' @param x Number to convert to rational fraction
 //' @param valid_min Lower bound
 //' @param valid_max Upper bound
 //'
 //' @return A data frame with results and metadata about the tree path.
 //' @export
 // [[Rcpp::export]]
DataFrame stern_brocot_cpp(const double x,
                        const double valid_min, const double valid_max) {

  double approximation;
  int cycles = 0;
  std::vector<int> path = {1};

  int left_num = floor(x), left_den = 1;
  int mediant_num = round(x), mediant_den = 1;
  int right_num = floor(x) + 1, right_den = 1;

  approximation = (double) mediant_num / mediant_den;
  const int insane = 1000;

  // Main computation loop for Stern-Brocot
  while ((approximation < valid_min) || (valid_max < approximation)) {
    double x0 = 2 * x - approximation;

    if (approximation < valid_min) {
      left_num = mediant_num;
      left_den = mediant_den;
      int k = floor(round_to_precision(right_num - x0 * right_den) / (x0 * left_den - left_num));
      right_num += k * left_num;
      right_den += k * left_den;
      path.push_back(0);
    } else {
      right_num = mediant_num;
      right_den = mediant_den;
      int k = floor(round_to_precision(x0 * left_den - left_num) / (right_num - x0 * right_den));
      left_num += k * right_num;
      left_den += k * right_den;
      path.push_back(1);
    }

    mediant_num = left_num + right_num;
    mediant_den = left_den + right_den;
    approximation = (double) mediant_num / mediant_den;

    cycles++;
    if (cycles > insane) {
      Rcpp::Rcout << "Cycle: " << cycles
                  << ", Approximation: " << approximation
                  << ", Bounds: [" << left_num << "/" << left_den << ", "
                  << right_num << "/" << right_den << "]\n";
      stop("STOP: too many cycles: " + std::to_string(cycles));
    }
  }

  if (mediant_den <= 0) stop("STOP: mediant_den is less than or equal to zero");

  // Ensure cycles == depth before returning
  if (cycles != path.size() - 1) {
    stop("STOP: cycles value does not match the depth. cycles: " + std::to_string(cycles) + ", path size: " + std::to_string(path.size()));
  }
  // Return the DataFrame with the estimated values and metrics calculated
  return create_stern_brocot_df(
    x, mediant_num, mediant_den, approximation,
    valid_min, valid_max, path
  );
}
