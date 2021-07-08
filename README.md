
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isocalcR

<!-- badges: start -->

# [![R-CMD-check](https://github.com/justinmathias/isocalcR/workflows/R-CMD-check/badge.svg)](https://github.com/justinmathias/isocalcR/actions) [![codecov](https://codecov.io/gh/justinmathias/isocalcR/branch/main/graph/badge.svg?token=W72B141APE)](https://codecov.io/gh/justinmathias/isocalcR)

<!-- badges: end -->

The goal of `isocalcR` is to provide a suite of user-friendly, open
source functions for commonly performed calculations when working with
stable isotope data. A major goal of `isocalcR` is to help eliminate
errors associated with data compilation necessary for many standard
calculations, as well as to provide the scientific community with a
reliable, easily accessible resource for reproducible work. Part of this
effort includes best practices of data usage, as the user is not
required to download atmospheric CO<sub>2</sub> or atmospheric
δ<sup>13</sup>CO<sub>2</sub> data for the workhorse calculations, but
instead relies on published, peer-reviewed, and recommended publicly
available data (Belmecheri and Lavergne, 2020).

## Installation

You can install the released version of isocalcR from
[CRAN](https://CRAN.R-project.org) with:

``` r
install.packages("isocalcR")
```

And the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("justinmathias/isocalcR")
```

## Functionality

| `isocalcR` Function | Description                                                                                                                                                                                |
|---------------------|--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------|
| `d13C.to.D13C`      | Calculate leaf carbon isotope discrimination (∆<sup>13</sup>C) given plant tissue δ<sup>13</sup>C signature (‰)                                                                            |
| `d13C.to.Ci`        | Calculate leaf intercellular CO<sub>2</sub> concentration (ppm) given plant tissue δ<sup>13</sup>C signature (‰)                                                                           |
| `d13C.to.CiCa`      | Calculate the ratio of leaf intercellular CO<sub>2</sub> to atmospheric CO<sub>2</sub> concentration (ppm) given plant tissue δ<sup>13</sup>C signature (‰)                                |
| `d13C.to.diffCaCi`  | Calculate the difference between atmospheric CO<sub>2</sub> concentration (ppm) and leaf intercellular CO<sub>2</sub> concentration (ppm) given plant tissue δ<sup>13</sup>C signature (‰) |
| `d13C.to.iWUE`      | Calculate leaf intrinsic water use efficiency (µmol CO<sub>2</sub> mol H<sub>2</sub>O<sup>-1</sup>) given plant tissue δ<sup>13</sup>C signature (‰)                                       |

<br>

## Examples

Calculate leaf intrinsic water use efficiency from leaf δ<sup>13</sup>C:

``` r
library(isocalcR) #Load the package
d13C.to.iWUE(-27, 2015, 300, 25) #Calculate iWUE from leaf organic material with a δ13C signature of -27 ‰ for the year 2015, 300 meters above sea level at 25°C.
#> [1] 94.4544
```

Data for atmospheric CO<sub>2</sub> and atmospheric
δ<sup>13</sup>CO<sub>2</sub> for the period 0 C.E. to 2019 C.E. can be
loaded and viewed. Data are from Belmecheri and Lavergne (2020).

``` r
data(CO2data)
head(CO2data)
#>   yr     Ca d13C.atm
#> 1  0 277.63    -6.41
#> 2  1 277.63    -6.41
#> 3  2 277.64    -6.41
#> 4  3 277.64    -6.41
#> 5  4 277.65    -6.41
#> 6  5 277.66    -6.41
```

## Literature cited

Belmecheri, S. & Lavergne, A. (2020). Compiled records of atmospheric
CO2 concentrations and stable carbon isotopes to reconstruct climate and
derive plant ecophysiological indices from tree rings.
Dendrochronologia, 63, 125748.