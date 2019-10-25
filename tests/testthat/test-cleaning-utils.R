test_that("Pad decimals works", {
  expect_equal(pad_decimals(2.11, 1), 2.1)
  expect_equal(pad_decimals(2.11, 0), 2)
})




test_that("Capitalize words works", {
  expect_equal(capitalize_words("hello world"), "Hello World")
  expect_equal(capitalize_words("HELLO WORLD"), "Hello World")
})




test_that("Fix column names works", {
  expect_equal(fix_column_names("hello_world"), "hello_world")
  expect_equal(fix_column_names("HELLO._.WORLD"), "hello_world")
})
