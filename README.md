
<!-- README.md is generated from README.Rmd. Please edit that file -->

# harpTobac

<!-- badges: start -->
<!-- badges: end -->

*harpTobac* is a an interface to the Python package,
[tobac](https://tobac.readthedocs.io/en/latest/index.html), for tracking
and object based analysis of clouds.

It currently enables you to pass `harp_grid_df` data frames to do a
basic analysis and tracking of 2d fields. That is, feature detection in
2d, segmentation in 2d and 2d linking of features into tracks.

Currently no specific functions for visualisation have been written, but
the outputs are basic data frames and `harp_grid_df` data frames that
can be easily visualised using [ggplot2](https://ggplot2.tidyverse.org/)
and [harpVis](https://harphub.github.io/harpVis/reference/index.html)
geoms.

## Installation

You can install harpTobac from Github like so:

``` r
if (!requireNamespace("remotes")) {
  install.packages("remotes")
}
remotes::install_github("harphub/harpTobac")
```

Once you have installed *harpTobac* you will need to install the *tobac*
Python package. This is done using the `install_tobac()` function. The
necessary packages will be installed into a *virtualenv* called
“harp-py”. This is where all Python packages that can be used by *harp*
are installed and run from. If you already have a “harp-py” *virtualenv*
you will be asked if you want to remove it prior to installation.
Typically you will answer no to this, otherwise all of your harp Python
packages will be removed.

``` r
library(harpTobac)
install_tobac()
```

You will only need to do the above once, straight after installing
*harpTobac*. Subsequently, when the *harpTobac* package is attached your
session will use the “harp-py” *virtualenv*.
