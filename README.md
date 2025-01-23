# SternBrocotR

The **SternBrocotR** R package provides tools for approximating real numbers as rational fractions using the Stern-Brocot tree. This package is ideal for applications requiring precise fractional approximations with configurable uncertainty bounds. It includes metadata about the approximation process, such as the depth of the Stern-Brocot tree traversal and binary path representation.

---

## Installation

You can install the latest version of **SternBrocotR** from GitHub:

```r
# Install the 'remotes' package if you don't have it
install.packages("remotes")

# Install SternBrocotR from GitHub
remotes::install_github("homeymusic/SternBrocotR")
```

---

## Usage

Here’s a quick example to get started:

### Approximate a Real Number to a Fraction
```r
library(SternBrocotR)

# Approximate 0.49 with symmetrical uncertainty
result <- stern_brocot(0.49, uncertainty = 0.03)
print(result)
# Output: A dataframe with the fraction 1/2 and additional metadata
```

### Approximate with Asymmetrical Uncertainty
```r
# Approximate 0.49 with different lower and upper uncertainty
result <- stern_brocot(0.49, uncertainty = c(0.04, 0.02))
print(result)
# Output: A dataframe with the fraction 1/2 and additional metadata
```

### Output Details
The result includes:
- `x`: The input number to approximate as a fraction
- `num`: Numerator of the fraction
- `den`: Denominator of the fraction
- `approximation`: The decimal approximation (`num / den`)
- `error`: Difference between the approximation and the input
- `valid_min` and `valid_max`: Bounds for the approximation
- `depth`: Depth of the Stern-Brocot tree traversal
- `path`: Binary representation of the tree path
- `path_id`: Binary path converted to an integer

---

## Citing SternBrocotR

If you use this package in your research or publications, please cite it as follows:

> **Brian McAuliff Mulloy** (2025). *SternBrocotR: Approximation of Real Numbers as Rational Fractions Using the Stern-Brocot Tree.* R package version 1.0.0. URL: [https://github.com/homeymusic/SternBrocotR](https://github.com/homeymusic/SternBrocotR)

You can also generate a citation in R:

```r
citation("SternBrocotR")
```

---

## Features

- Approximate real numbers with configurable uncertainty bounds.
- Compute fractional approximations with metadata like depth, error, and binary path representation.
- Efficient implementation using the Stern-Brocot tree algorithm.

---

## References

This package implements concepts and methods inspired by the following papers:

1. Forišek, M. (2007). Approximating rational numbers by fractions. In *International Conference on Fun with Algorithms* (pp. 156-165). Springer Berlin Heidelberg. [DOI](https://doi.org/10.1007/978-3-540-72914-3_16)
2. Stolzenburg, F. (2015). Harmony perception by periodicity detection. *Journal of Mathematics and Music, 9*(3), 215-238. [DOI](https://doi.org/10.1080/17459737.2015.1042187)

---

## License

This package is licensed under the MIT License. See the [LICENSE](./LICENSE.md) file for more details.

---

## Contributing

Contributions are welcome! If you find a bug or have an idea for a new feature, please open an issue or submit a pull request on [GitHub](https://github.com/homeymusic/SternBrocotR).

---

## Acknowledgments

Special thanks to the R and C++ communities for creating tools like **Rcpp**, which make packages like this possible.
