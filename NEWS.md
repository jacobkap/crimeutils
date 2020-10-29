# crime 0.2.0

* Adds a function to that returns a data.frame from a regression model
so it's easier to turn into tables for publication. Also adds a function
that graphs the coefficient value and the 95% confidence interval
for all coefficients.
* Updates the LaTeX table generator code to improve formatting.
* Fixed bug in `pad_decimals()` that incorrectly dropped the padding when converting back to a number. Now
returns data as strings.

# crime 0.1.0

* Added a `NEWS.md` file to track changes to the package.
