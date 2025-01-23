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
