test_that("close to 0.5 returns 1/2 with symmetrical uncertainty", {
  real = 0.49
  uncertainty = 0.03
  result <- stern_brocot(
    x = real,
    uncertainty = uncertainty
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
  expect_equal(result$path, '10')
  expect_equal(result$path_id, 2)
  expect_equal(result$depth, 2)
})
test_that("close to 0.5 returns 1/2 with asymmetrical uncertainty", {
  real = 0.49
  lower_uncertainty = 0.04
  upper_uncertainty = 0.02
  result <- stern_brocot(
    x = real,
    uncertainty = c(lower_uncertainty, upper_uncertainty)
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})
test_that("3 unceratinties throws an error", {
  real = 0.49
  lower_uncertainty = 0.04
  nonsense_uncertainty = 0.03
  upper_uncertainty = 0.02
  expect_error(stern_brocot(
    x = real,
    uncertainty = c(lower_uncertainty, nonsense_uncertainty, upper_uncertainty)
  ))
})
test_that("result$num can be any integer and result$den must be a positive integer", {
  real = 0.49
  uncertainty = 0.03
  result <- stern_brocot(
    x = real,
    uncertainty = uncertainty
  )
  expect_true(is.integer(result$num)) # num can be any integer
  expect_true(is.integer(result$den)) # den must be an integer
  expect_true(result$den > 0)         # den must be positive
})
test_that("x = 0 returns 0/1", {
  real = 0
  uncertainty = 0.01
  result <- stern_brocot(
    x = real,
    uncertainty = uncertainty
  )
  expect_equal(result$num, 0)
  expect_equal(result$den, 1)
})
test_that("x = 1 returns 1/1", {
  real = 1
  uncertainty = 0.01
  result <- stern_brocot(
    x = real,
    uncertainty = uncertainty
  )
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)
})
test_that("negative input returns a negative numerator", {
  real = -0.5
  uncertainty = 0.01
  result <- stern_brocot(
    x = real,
    uncertainty = uncertainty
  )
  expect_equal(result$num, -1)
  expect_equal(result$den, 2)
})

GABOR_UNCERTAINTY = 1 / (4 * pi)

test_that("0.5 returns 1/2", {
  result <- stern_brocot(1/2, GABOR_UNCERTAINTY)
  expect_equal(result$num, 1)
  expect_equal(result$den, 2)
})

test_that("29 / 42 works", {
  result <- stern_brocot(29 / 42, GABOR_UNCERTAINTY ^ 2)
  expect_equal(result$num, 9)
  expect_equal(result$den, 13)
})

expected_stern_brocot_columns <- c(
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

test_that("if x is less than unceratinty it returns interesting stuff", {

  result <- stern_brocot(1, 2)
  expect_s3_class(result, "data.frame")  # Expect a data frame
  expect_equal(names(result), expected_stern_brocot_columns)
  expect_equal(result$num, 1)
  expect_equal(result$den, 1)

  result <- stern_brocot(50.234, 60)
  expect_equal(names(result), expected_stern_brocot_columns)
  expect_equal(result$num, 50)
  expect_equal(result$den, 1)

  result <- stern_brocot(0.25, 0.5)
  expect_equal(names(result), expected_stern_brocot_columns)
  expect_equal(result$num, 0)
  expect_equal(result$den, 1)

})

test_that("stern_brocot function returns correct rational approximation", {
  x = 2.5
  uncertainty = 0.01
  # Test case 1: Standard input with small uncertainty
  result <- stern_brocot(x, uncertainty)

  # Check if the approximation is reasonable
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - x) <= uncertainty)
  expect_equal(result$approximation, approx_value)
  expect_equal(result$error, 0)

  # Test case 2: Edge case for small x
  uncertainty = 0.0001
  result <- stern_brocot(0.001, uncertainty)
  expect_equal(result$num, 1)
  expect_true(result$den > 900)
  expect_equal(result$x, 0.001)

  # Test case 3: Large x value with moderate uncertainty
  uncertainty = 0.1
  result <- stern_brocot(100.75, uncertainty)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 100.75) <= uncertainty)

  # Test case 4: Small uncertainty should result in very accurate fraction
  uncertainty = 0.001
  result <- stern_brocot(3.333, uncertainty)
  approx_value <- result$num / result$den
  expect_true(abs(approx_value - 3.333) <= uncertainty)

  # Test case 7: Result should be an integer if x is an integer
  uncertainty = 0.1
  result <- stern_brocot(3, uncertainty)
  expect_equal(result$num, 3)
  expect_equal(result$den, 1)
  expect_equal(result$x, 3)
})

test_that("stern_brocot does not return zero numerator or denominator", {
  x <- 0.1666667
  uncertainty <- 3.0

  result <- stern_brocot(x, uncertainty)

  expect_true(result$num == 0)
  expect_true(result$den != 0, info = "Stern-Brocot should never return a 0 denominator")
  expect_equal(result$x, x, info = "The original value should match the input")

  expect_equal(result$approximation, 0)
  expect_equal(result$error, -x)

})

test_that("depth_cpp computes correct values", {
  irrational = stern_brocot(sqrt(2), GABOR_UNCERTAINTY)
  expect_equal(irrational$depth, 4)
  expect_equal(irrational$path, "1010")
  expect_equal(irrational$path_id, 10)
})
