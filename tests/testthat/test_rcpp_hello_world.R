test_that('hello world works', {
 expect_equal(rcpp_hello_world()[[1]][1], 'foo')
 expect_equal(rcpp_hello_world()[[1]][2], 'bar')
})
