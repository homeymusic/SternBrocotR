GABOR_UNCERTAINTY = 1 / (4 * pi)

expected_stern_brocot_cpp_columns <- c(
  "x",
  "num",
  "den",
  "approximation",
  "error",
  "valid_min",
  "valid_max",
  "depth",
  "path",
  "path_id"
)


# Test with scalar x and scalar bounds
test_that("stern_brocot_cpp handles scalar inputs correctly", {
  x <- 1.667
  result <- stern_brocot_cpp(x, x - GABOR_UNCERTAINTY, x + GABOR_UNCERTAINTY)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "1101")
  expect_equal(result$path_id, 13)
})

# Test with vector x and scalar bounds
test_that("stern_brocot_cpp handles vector x with scalar bounds", {
  x <- c(1.667, sqrt(2))
  result <- stern_brocot_cpp(x, x - GABOR_UNCERTAINTY, x + GABOR_UNCERTAINTY)
  expect_equal(result$num, c(5, 7))
  expect_equal(result$den, c(3, 5))
  expect_equal(result$depth, c(4, 5))
  expect_equal(result$path, c("1101", "11001"))
  expect_equal(result$path_id, c(13, 25))
})

# Test with vector x and vector bounds
test_that("stern_brocot_cpp handles vector x with vector bounds", {
  x <- c(1.667, sqrt(2))
  valid_min <- c(1.6, 1.4)
  valid_max <- c(1.8, 1.6)
  result <- stern_brocot_cpp(x, valid_min, valid_max)
  expect_equal(result$num, c(5, 3))
  expect_equal(result$den, c(3, 2))
  expect_equal(result$depth, c(4, 3))
  expect_equal(result$path, c("1101", "110"))
  expect_equal(result$path_id, c(13, 6))
  expect_equal(result$valid_min, valid_min)
  expect_equal(result$valid_max, valid_max)
})

# Test for mismatched vector lengths
test_that("stern_brocot_cpp errors on mismatched valid min and max lengths", {
  x <- c(1.667, sqrt(2))
  valid_min <- c(1.6)  # Length 1
  valid_max <- c(1.8, 1.6)  # Length 2
  expect_error(
    stern_brocot_cpp(x, valid_min, valid_max),
    "valid_min and valid_max must be the same length"
  )
})

# Test for mismatched vector lengths
test_that("stern_brocot_cpp errors on mismatched lengths", {
  x <- c(1.667, sqrt(2))
  valid_min <- c(1.6, 1.5, 1.4)  # Length 3
  valid_max <- c(1.8, 1.6, 1.9)  # Length 3
  expect_error(
    stern_brocot_cpp(x, valid_min, valid_max),
    "valid_min and valid_max must either be of length 1 or match the length of x"
  )
})

# Test for invalid bounds
test_that("stern_brocot_cpp errors on invalid bounds", {
  x <- c(1.667, sqrt(2))

  # Invalid valid_min (negative value)
  valid_min <- c(-1, 1.4)
  valid_max <- c(1.8, 1.6)
  expect_error(
    stern_brocot_cpp(x, valid_min, valid_max),
    "valid_min must be greater than 0"
  )

  # Invalid valid_max (less than x)
  valid_min <- c(1.6, 1.4)
  valid_max <- c(1.6, 1.2)
  expect_error(
    stern_brocot_cpp(x, valid_min, valid_max),
    "STOP: x must be less than valid_max"
  )
})

test_that("depth_cpp computes correct values", {
  x = 1.667
  result = stern_brocot_cpp(x, x - GABOR_UNCERTAINTY, x + GABOR_UNCERTAINTY)
  expect_equal(result$num, 5)
  expect_equal(result$den, 3)
  expect_equal(result$depth, 4)
  expect_equal(result$path, "1101")
  expect_equal(result$path_id, 13)

  x = sqrt(2)
  result = stern_brocot_cpp(x, x - GABOR_UNCERTAINTY, x + GABOR_UNCERTAINTY)
  expect_equal(result$num, 7)
  expect_equal(result$den, 5)
  expect_equal(result$depth, 5)
  expect_equal(result$path, "11001")
  expect_equal(result$path_id, 25)
})

test_that("close to 0.5 returns 1/2 with symmetrical uncertainty", {
  real = 0.49
  uncertainty = 0.03
  result <- stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
  expect_equal(result$path, '10')
  expect_equal(result$path_id, 2)
  expect_equal(result$depth, 2)

  real = -real
  expect_error(stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  ))
})
test_that("close to 0.5 returns 1/2 with asymmetrical uncertainty", {
  real = 0.49
  lower_uncertainty = 0.04
  upper_uncertainty = 0.02
  result <- stern_brocot_cpp(
    x = real,
    valid_min = real-lower_uncertainty,
    valid_max = real+upper_uncertainty
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})
test_that("error with uncertainties out of bounds", {
  real = 0.49
  lower_uncertainty = -0.04
  upper_uncertainty = 0.02
  expect_error(stern_brocot_cpp(
    x = real,
    valid_min = real-lower_uncertainty,
    valid_max = real+upper_uncertainty
  ))
})
test_that("x <=  valid_min", {
  expect_error(stern_brocot_cpp(
    x = 0.01,
    valid_min = 0.1,
    valid_max = 0.2
  ))
})
test_that("valid_max <=", {
  expect_error(stern_brocot_cpp(
    x = 0.3,
    valid_min = 0.1,
    valid_max = 0.2
  ))
})
test_that("result$num can be any integer and result$den must be a positive integer", {
  real = 0.49
  uncertainty = 0.03
  result <- stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  )
  expect_true(is.integer(result$num)) # num can be any integer
  expect_true(result$num > 0)         # num must be positive
  expect_true(is.integer(result$den)) # den must be an integer
  expect_true(result$den > 0)         # den must be positive
})
test_that("valid min must be greater than zero", {
  real = 0
  uncertainty = 0.01
  expect_error(stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  ),'STOP: valid_min must be greater than 0')
})
test_that("x = 1 returns 1/1", {
  real = 1
  uncertainty = 0.01
  result <- stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)
})
test_that("error when less than 0", {
  real = -1
  uncertainty = 0.01
  expect_error(stern_brocot_cpp(
    x = real,
    valid_min = real-uncertainty,
    valid_max = real+uncertainty
  ),'STOP: valid_min must be greater than 0')
})

test_that("0.5 returns 1/2", {
  result <- stern_brocot_cpp(1/2,
                             1/2 - GABOR_UNCERTAINTY,
                             1/2 + GABOR_UNCERTAINTY)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})

test_that("29 / 42 works", {
  x = 29 / 42
  result <- stern_brocot_cpp(29 / 42,
                             x - GABOR_UNCERTAINTY ^ 2,
                             x + GABOR_UNCERTAINTY ^ 2)
  expect_equal(result$num, 9)
  expect_equal(result$den, 13)
})


test_that("if x is less than unceratinty it returns interesting stuff", {
  x = 6
  result <- stern_brocot_cpp(x, x-2, x+2)
  expect_s3_class(result, "data.frame")  # Expect a data frame
  expect_equal(names(result), expected_stern_brocot_cpp_columns)
  expect_equal(result$num, 4)
  expect_equal(result$den, 1)

  x = 50.234
  result <- stern_brocot_cpp(x, x-1, x+1)
  expect_equal(names(result), expected_stern_brocot_cpp_columns)
  expect_equal(result$num, 50)
  expect_equal(result$den, 1)

  x = 1.25
  result <- stern_brocot_cpp(x, x-0.5, x+0.5)
  expect_equal(names(result), expected_stern_brocot_cpp_columns)
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)

})

test_that("stern_brocot_cpp function returns correct rational approximation", {
  x = 2.5
  uncertainty = 0.01
  # Test case 1: Standard input with small uncertainty
  result <- stern_brocot_cpp(x, x-uncertainty, x+uncertainty)

  # Check if the approximation is reasonable
  approx_value <- result$num / result$den
  expect_equal(result$num,5)
  expect_equal(result$den,2)
  expect_true(abs(approx_value - x) <= uncertainty)
  expect_equal(result$approximation, approx_value)
  expect_equal(result$error, 0)

  # Test case 2: Edge case for small x
  x = 0.001
  uncertainty = 0.0001
  result <- stern_brocot_cpp(x, x-uncertainty, x+uncertainty)
  expect_equal(result$num, 1)
  expect_equal(result$den, 910)
  expect_equal(result$x, x)
  expect_equal(result$depth, 910)

  # Test case 3: Large x value with moderate uncertainty
  x = 100.75
  uncertainty = 0.1
  result <- stern_brocot_cpp(x, x-uncertainty, x+uncertainty)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 100.75) <= uncertainty)

  # Test case 4: Small uncertainty should result in very accurate fraction
  x = 3.333
  uncertainty = 0.001
  result <- stern_brocot_cpp(3.333, x-uncertainty, x+uncertainty)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 3.333) <= uncertainty)

  # Test case 7: Result should be an integer if x is an integer
  x = 3
  uncertainty = 0.1
  result <- stern_brocot_cpp(x, x-uncertainty, x+uncertainty)
  expect_equal(result$num, 3)
  expect_equal(result$den, 1)
  expect_equal(result$x, 3)
})

test_that("stern_brocot_cpp does not return zero numerator or denominator", {
  x <- 10.1666667
  uncertainty <- 3.0

  result <- stern_brocot_cpp(x, x-uncertainty, x+uncertainty)

  expect_true(result$num == 8)
  expect_true(result$den != 0, info = "Stern-Brocot should never return a 0 denominator")
  expect_equal(result$x, x, info = "The original value should match the input")

  expect_equal(result$approximation, 8)
  expect_equal(result$error, 8-x)

})
