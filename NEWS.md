# tind 0.2.4

## New Features

- Added support for class `hms` from package of the same name (conversion to/from).

## Improvements

- `as.tind` method for multi-column data frames is significantly faster in the following cases:
    - forwards to `tind` constructor when all columns are numeric,
    - forwards to `date_time` when there are two columns and one represents dates and the other times of day.


# tind 0.2.3

First CRAN release.
