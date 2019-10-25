test_that("Pad decimals works", {
  expect_equal(pad_decimals(2.11, 1), 2.1)
  expect_equal(pad_decimals(2.11, 0), 2)


  expect_equal(pad_decimals(2.11, 2), 2.11)
  expect_equal(pad_decimals(2.11, 3), 2.110)
  expect_equal(pad_decimals(2.11, 7), 2.1100000)
  expect_equal(pad_decimals("2.11", 1), 2.1)
  expect_equal(pad_decimals(c(2.3, 8, 6.789)), c(2.300, 8.000, 6.789))
  expect_equal(pad_decimals(c(2.3, 8, 6.789), 0), c(2, 8, 7))

  expect_equal(pad_decimals(c(2.3, 8.5, 8.4, 7.5, 6.789), 0), c(2, 8, 8, 8, 7))
  expect_equal(pad_decimals(c(2.3, 8.5, 8.4, 7.5, 6.789), 2),
               c(2.30, 8.50, 8.40, 7.50, 6.79))
  expect_equal(pad_decimals(as.character(c(2.3, 8.5, 8.4, 7.5, 6.789)), 2),
               c(2.30, 8.50, 8.40, 7.50, 6.79))

})

test_that("Capitalize words works", {
  expect_equal(capitalize_words("hello world"), "Hello World")
  expect_equal(capitalize_words("HELLO WORLD"), "Hello World")

  expect_equal(capitalize_words("district of columbia"), "District of Columbia")
  expect_equal(capitalize_words("district Of columbia", lowercase_of = FALSE),
               "District Of Columbia")
  expect_equal(capitalize_words("DISTRICT OF COLUMBIA"), "District of Columbia")
  expect_equal(capitalize_words("DISTRICT OF COLUMBIA", lowercase_of = FALSE),
               "District Of Columbia")

  expect_equal(capitalize_words(c("hello darkness MY", "old fRiend")),
               c("Hello Darkness My", "Old Friend"))


})




test_that("Fix column names works", {
  expect_equal(fix_column_names("hello_world"), "hello_world")
  expect_equal(fix_column_names("HELLO._.WORLD"), "hello_world")
})
