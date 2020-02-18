test <- data.frame(words = c("Four score and", "seven", "years ago",
                              "out fathers", "BROUGHT FOrTH November", "1863"),
                   numbers = c(1, 2, 3.456, 9.9, 0.00003, 108.0054),
                   stringsAsFactors = FALSE)
test$words_factor <- as.factor(test$words)
test$numbers_characters <- as.character(test$numbers)

test_that("Pad decimals works", {
  expect_equal(pad_decimals(2.11, 1), 2.1)
  expect_equal(pad_decimals(2.11, 0), 2)

  expect_equal(pad_decimals(2, 2), 2.00)
  expect_equal(pad_decimals(2, 1), 2.0)
  expect_equal(pad_decimals(2, 5), 2.00000)
  expect_equal(pad_decimals(c(2, 3.11), 2), c(2.00, 3.11))


  expect_equal(pad_decimals(2.11, 2), 2.11)
  expect_equal(pad_decimals(2.11, 3), 2.110)
  expect_equal(pad_decimals(2.11, 7), 2.1100000)
  expect_error(pad_decimals("2.11", 1))
  expect_equal(pad_decimals(c(2.3, 8, 6.789)), c(2.300, 8.000, 6.789))
  expect_error(pad_decimals(as.factor(c(2.3, 8, 6.789))))
  expect_error(pad_decimals(c("2.3", "8", "6.789")))
  expect_equal(pad_decimals(c(2.3, 8, 6.789), 0), c(2, 8, 7))

  expect_equal(pad_decimals(c(2.3, 8.5, 8.4, 7.5, 6.789), 0), c(2, 8, 8, 8, 7))
  expect_equal(pad_decimals(c(2.3, 8.5, 8.4, 7.5, 6.789), 2),
               c(2.30, 8.50, 8.40, 7.50, 6.79))



  expect_type(pad_decimals(2.11, 2), "double")
  expect_type(pad_decimals(2.11, 3), "double")
  expect_type(pad_decimals(2.11, 7), "double")
  expect_type(pad_decimals(c(2.3, 8, 6.789)), "double")


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


  expect_equal(capitalize_words("hello-world"), "Hello-World")
  expect_equal(capitalize_words("hello'world"), "Hello'World")

  expect_equal(upper_first_letter("hello world"), "Hello world")
  expect_equal(upper_first_letter("hELLo woRLD"), "HELLo woRLD")
  expect_equal(upper_first_letter(c("hello darkness MY", "old fRiend")),
               c("Hello darkness MY", "Old fRiend"))

  expect_equal(upper_first_letter(1:5), as.character(1:5))
  expect_equal(capitalize_words(1:5), as.character(1:5))

})

test_that("Fix make state abbreviations works", {
  expect_true(is.na(make_state_abb("hello_world")))
  expect_equal(make_state_abb(c("CALIFORNIA", "UTAH")), c("CA", "UT"))
  expect_equal(make_state_abb(c("CALIFORNIA",
                                "UTAH",
                                2,
                                NA,
                                "hello",
                                "PENNSYLVANIA",
                                "pennsylvania",
                                "ppennsylvania",
                                "PeNNsylvaNIA")), c("CA",
                                                    "UT",
                                                   NA,
                                                   NA,
                                                   NA,
                                                   "PA",
                                                   "PA",
                                                   NA,
                                                   "PA"))

  expect_true(all(is.na(make_state_abb(1:5))))
})


test_that("Fix column names works", {
  expect_equal(fix_column_names("hello_world"), "hello_world")
  expect_equal(fix_column_names("HELLO._.WORLD"), "hello_world")

  expect_equal(fix_column_names(1:5), as.character(1:5))
})
