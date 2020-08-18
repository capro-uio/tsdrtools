
<!-- README.md is generated from README.Rmd. Please edit that file -->

# tsdrtools <img src='man/figures/logo.png' align="right" height="138.5" />

<!-- badges: start -->

[![R build
status](https://github.com/LCBC-UiO/tsdrtools/workflows/R-CMD-check/badge.svg)](https://github.com/LCBC-UiO/tsdrtools/actions)
[![Travis build
status](https://travis-ci.com/LCBC-UiO/tsdrtools.svg?branch=master)](https://travis-ci.com/LCBC-UiO/tsdrtools)
[![AppVeyor build
status](https://ci.appveyor.com/api/projects/status/github/LCBC-UiO/tsdrtools?branch=master&svg=true)](https://ci.appveyor.com/project/LCBC-UiO/tsdrtools)
<!-- [![Codecov test coverage](https://codecov.io/gh/LCBC-UiO/tsdrtools/branch/master/graph/badge.svg)](https://codecov.io/gh/LCBC-UiO/tsdrtools?branch=master) -->
[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://www.tidyverse.org/lifecycle/#experimental)
[![CRAN
status](https://www.r-pkg.org/badges/version/tsdrtools)](https://CRAN.R-project.org/package=tsdrtools)
<!-- badges: end -->

The goal of tsdrtools is to make it easier for R users of TSD to install
packages that fail install using the TSD mini-CRAN. Certain R-packages
require compilation outside of TSD before they can be installed, and as
such can be burdensom for some users to administer.

Since packages often have a muriad of dependencies, this process can
becomes increasingly cumbersome, as all these extra packages also need
to be obtained, which again might require special compilation.

In the two step procedure here, a package is downloaded together with
all dependent packages, zipped into an archive for easy import into TSD,
where it can be unzipped and installed using a special function.

## Installation

You can install the development version of tsdrtools

``` r
install.packages("remotes")
remotes::install_github("LCBC-UIO/tsdrtools")
```

for installation of this package inside TSD, download the newest source
file from <https://github.com/LCBC-UiO/tsdrtools/>.

Transfer the package to TSD (import it), and install as a source
package.

``` r
install.packages("/path/to/tsdrtools.tar.gz", repo = NULL)
```

## Example

This package contains two main functions:

1.  `tsd_package_prepare()` is used outside of TSD, with internet
    connection to prepare a package for install within TSD.  
2.  Once step 1 is completed and data transfered to TSD, the
    `tsd_package_install()` is used within TSD to install the package
    within TSD.

<!-- end list -->

``` r
library(tsdrtools)

# outside TSD
tsd_package_prepare("dplyr")

# within TSD
tsd_package_prepare("dplyr.zip")
```

## Limitations

Currently the package will only install packages that are available on
CRAN. We are working on a version that might allow downloading and
making available packages from other repositories, but that is more
difficult.

## Bug reports

Report bugs using [github
issues](https://github.com/LCBC-UiO/tsdrtools/issues)

## Code of Conduct

Please note that the tsdrtools project is released with a [Contributor
Code of
Conduct](https://contributor-covenant.org/version/2/0/CODE_OF_CONDUCT.html).
By contributing to this project, you agree to abide by its terms.
