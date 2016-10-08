# The ideal dataset is both:
#   1. Easy to analyze for researcher
#     1a. Statistical analysis is easily and quickly done without
#         further processing.
#     1b. Graphs can be quickly made.
#     1c. Data is easy to work with (e.g. column names are concise
#                                    and intuitive)
#     1d. The researcher can begin using the data by analyzing it -
#         not by cleaning it.
#   2. Understandable to the general public
#     2a. Column names are concise and informative
#     2b. One idea (topic) per column (e.g. if you have race and gender,
#                                      each get their own column)
#     2c. One event per row row
#     2d. Values are understandable to non-practictioner (e.g. use "female"
#                                                         instead of "f")
#
#
# Every dataset should be standardized so all datasets work and look
# the same. This way it is easier to compare two (or more) datasets and
# a user only needs to become familiar with one to be able to use all off
# of them. With the current diversity of dataset formats, users must spend
# time familiarizing themselves with that dataset. This is a waste of time.
#
#
# Data should be in its proper type. Character type should be character.
# Numeric should be numeric. There is nearly no reason to use factor type.
#
# Coordinates should be in their respective columns (longitude gets its
#                                                    own column as does
#                                                    latitude.).
# Longtiude and latitude should never be in the same column together!
#
# Avoid all abreviation for race/ethnicity and gender. "Black" is more clear
# than "B"; "Female" is more clear than "f".
#
# Dates should be formatted using lubridate()
#
# All punctuation (except from _ to separate words) should be removed from
# column names.
#
# Unnecessary columns should be removed.
#
# The best dataset is the smallest, clearest one possible.
