# crimeutils 0.3.0

* Minor bug fixes.
* Fixes typo in make_average_linegraph and error when using tibbles rather than
data.frames.
* Changes capitalize_words so it doesn't capitalize the letter after an apostrophe.
* Changes `make_average_linegraph()` to `make_average_graph()` which can now also 
  return a barplot. This function now can include 95% confidence intervals for 
  both linegraphs and barplots.
* Add functions `make_mean_std_dev_by_group_table()` and `make_n_and_percent_table()`
  functions for easy descriptive stats tables. 
* Make LaTeX table rows have more space in between them.

# crimeutils 0.2.1

* Fix issue in `make_barplots()` when using a tibble where the plot would show only 
a NA column instead of the actual categories. Fixes this by converting the
data to a data.frame.

# crimeutils 0.2.0

* Adds a function to that returns a data.frame from a regression model
so it's easier to turn into tables for publication. Also adds a function
that graphs the coefficient value and the 95% confidence interval
for all coefficients.
* Updates the LaTeX table generator code to improve formatting.
* Fixed bug in `pad_decimals()` that incorrectly dropped the padding when converting back to a number. Now
returns data as strings.

# crimeutils 0.1.0

* Added a `NEWS.md` file to track changes to the package.
