
<!-- README.md is generated from README.Rmd. Please edit that file -->

# isocalcR

<!-- badges: start -->

# [![R-CMD-check](https://github.com/justinmathias/isocalcR/workflows/R-CMD-check/badge.svg)](https://github.com/justinmathias/isocalcR/actions) [![codecov](https://codecov.io/gh/justinmathias/isocalcR/branch/main/graph/badge.svg?token=W72B141APE)](https://app.codecov.io/gh/justinmathias/isocalcR) [![metacran downloads](https://cranlogs.r-pkg.org/badges/isocalcR)](https://cran.r-project.org/package=isocalcR) [![license](https://img.shields.io/badge/license-GPL--3-blue.svg)](https://www.gnu.org/licenses/gpl-3.0.en.html) [![CRAN\_Status\_Badge](https://www.r-pkg.org/badges/version/isocalcR)](https://cran.r-project.org/package=isocalcR)

<!-- badges: end -->

      The goal of `isocalcR` is to provide a suite of user-friendly,
open source functions for commonly performed calculations when working
with stable isotope data. A major goal of `isocalcR` is to help
eliminate errors associated with data compilation necessary for many
standard calculations, as well as to provide the scientific community
with a reliable, easily accessible resource for reproducible work. Part
of this effort includes best practices of data usage, as the user is not
required to download atmospheric CO<sub>2</sub> or atmospheric
δ<sup>13</sup>CO<sub>2</sub> data for the workhorse calculations, but
instead relies on published, peer-reviewed, and recommended publicly
available data (Belmecheri and Lavergne, 2020). `isocalcR` is not meant
to replace an understanding of the underlying physiological mechanisms
related to these calculations, but instead to streamline the process. At
present, calculations for years 0 C.E. - 2021 C.E. are stable and will
work with all functions, with 2022 being added at the end of the year.

      `isocalcR` 0.0.1 and `isocalcR` 0.0.2 incorporated
photorespiratory processes into calculations where C<sub>i</sub> was
computed. The current release version of `isocalcR`, 0.1.0, now has the
option to specify the formulation used in calculating physiological
indices where C<sub>i</sub> is necessary for calculations (i.e. CiCa,
diffCaCi, iWUE). Furthermore, `isocalcR` now includes the function
“custom.calc”, which allows the user to specify variables such as
atmospheric \[CO2\], that are automatically determined in other
functions.

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
| `custom.calc`       | Calculate ∆<sup>13</sup>C, Ci, CiCa, diffCaCi, or iWUE given plant tissue δ<sup>13</sup>C signature (‰)                                                                                    |

<br>

## Examples

Calculate leaf intrinsic water use efficiency from leaf δ<sup>13</sup>C:

``` r
library(isocalcR) #Load the package
library(tidyverse) #Load the tidyverse
#> ── Attaching packages ─────────────────────────────────────── tidyverse 1.3.1 ──
#> ✔ ggplot2 3.3.6     ✔ purrr   0.3.4
#> ✔ tibble  3.1.7     ✔ dplyr   1.0.8
#> ✔ tidyr   1.2.0     ✔ stringr 1.4.0
#> ✔ readr   2.1.2     ✔ forcats 0.5.1
#> Warning: package 'tidyr' was built under R version 4.0.5
#> Warning: package 'readr' was built under R version 4.0.5
#> Warning: package 'dplyr' was built under R version 4.0.5
#> ── Conflicts ────────────────────────────────────────── tidyverse_conflicts() ──
#> ✖ dplyr::filter() masks stats::filter()
#> ✖ dplyr::lag()    masks stats::lag()

#Calculate iWUE from leaf organic material with a δ13C signature of -27 ‰ for the year 2015, 300 meters above sea level at 25°C.
d13C.to.iWUE(d13C.plant = -27, 
             year = 2015, 
             elevation = 300, 
             temp = 25) 
#> [1] 87.58236

#Use custom.calc to calculate iWUE from the same leaf sample as above.
custom.calc(d13C.plant = -27,
            d13C.atm = -8.44,
            outvar = "iWUE",
            Ca = 399.62,
            elevation = 300,
            temp = 25)
#> [1] 87.58236

#Calculate the ratio of leaf intercellular to atmospheric CO2 (Ci/Ca) using the simple formulation for leaf and wood. Internally updates apparent fractionation by Rubisco, b, according to Cernusak and Ubierna 2022.
d13C.to.CiCa(d13C.plant = -27,
             year = 2015,
             elevation = 300,
             temp = 25,
             tissue = "leaf")
#> [1] 0.6493374

d13C.to.CiCa(d13C.plant = -27,
             year = 2015,
             elevation = 300,
             temp = 25,
             tissue = "wood")
#> [1] 0.6954988

#Calculate iWUE using the "simple", "photorespiration", and "mesophyll" formulations.
d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "simple")
#> [1] 75.99569

d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "photorespiration")
#> [1] 75.35815

d13C.to.iWUE(d13C.plant = -28,
             year = 2015,
             elevation = 300,
             temp = 15,
             method = "mesophyll")
#> [1] 44.00744
```

Data for atmospheric CO<sub>2</sub> and atmospheric
δ<sup>13</sup>CO<sub>2</sub> for the period 0 C.E. to 2021 C.E. can be
loaded and viewed. Data are from Belmecheri and Lavergne (2020).

``` r
data(CO2data) #Load CO2data into your environment

head(CO2data, 10) #View initial CO2data observations
#>    yr     Ca d13C.atm
#> 1   0 277.63    -6.41
#> 2   1 277.63    -6.41
#> 3   2 277.64    -6.41
#> 4   3 277.64    -6.41
#> 5   4 277.65    -6.41
#> 6   5 277.66    -6.41
#> 7   6 277.66    -6.41
#> 8   7 277.67    -6.41
#> 9   8 277.67    -6.41
#> 10  9 277.68    -6.41

tail(CO2data, 10) #View most recent CO2data observations
#>        yr     Ca d13C.atm
#> 2012 2011 390.65    -8.33
#> 2013 2012 392.77    -8.36
#> 2014 2013 395.16    -8.39
#> 2015 2014 397.54    -8.42
#> 2016 2015 399.62    -8.44
#> 2017 2016 402.82    -8.48
#> 2018 2017 405.13    -8.51
#> 2019 2018 407.14    -8.55
#> 2020 2019 410.04    -8.58
#> 2021 2020 413.95    -8.61
```

Data for piru13C can loaded, viewed, and used to calculate iWUE for each
formulation. Data are from Mathias and Thomas (2018).

``` r
data(piru13C)
head(piru13C)
#> # A tibble: 6 × 10
#> # Groups:   Year [2]
#>    Year Site  wood.d13C MGT_C Elevation_m  frac iWUE_simple iWUE_photorespirati…
#>   <dbl> <chr>     <dbl> <dbl>       <dbl> <dbl>       <dbl>                <dbl>
#> 1  1940 CGL       -23.2  17.4        1206     2        81.7                 67.3
#> 2  1940 MCG       -24.0  17.4        1060     2        73.4                 59.9
#> 3  1940 SOR       -24.5  17.4        1033     2        69.2                 56.1
#> 4  1941 CGL       -22.9  18.6        1206     2        84.4                 69.1
#> 5  1941 MCG       -23.6  18.6        1060     2        77.5                 63.0
#> 6  1941 SOR       -24.4  18.6        1033     2        70.4                 56.5
#> # … with 2 more variables: iWUE_mesophyll <dbl>, iWUE_mesophylll <dbl>


#Calculate iWUE from tree ring (wholewood) d13C from Mathias and Thomas (2018) 
#using previously loaded piru13C data

#First drop years where there are no data
piru13C <- piru13C %>% 
  drop_na() 

#Calculate iWUE for each case using 'mapply'
piru13C$iWUE_simple <- mapply(d13C.to.iWUE, #Call the function
                              d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                              year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                              elevation = piru13C$Elevation_m, #Assign the elevation
                              temp = piru13C$MGT_C, #Assign the temperature 
                              method = "simple", #Specify the method
                              tissue = "wood") #Specify which tissue the sample is from

piru13C$iWUE_photorespiration <- mapply(d13C.to.iWUE, #Call the function
                                        d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                                        year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                                        elevation = piru13C$Elevation_m, #Specify elevation
                                        temp = piru13C$MGT_C, #Specify the temperature during tissue formation
                                        method = "photorespiration", #Specify the iWUE calculation formulation
                                        frac = piru13C$frac) #Specify any post-photosynthetic fractionations. In this case 2 permille to account for leaf to wood.

piru13C$iWUE_mesophyll <- mapply(d13C.to.iWUE, #Call the function
                                        d13C.plant = piru13C$wood.d13C, #Assign the plant d13C value
                                        year = piru13C$Year, #Assign the year to match atmospheric CO2 and atmospheric d13CO2
                                        elevation = piru13C$Elevation_m, #Specify elevation
                                        temp = piru13C$MGT_C, #Specify the temperature during tissue formation
                                        method = "mesophyll", #Specify the iWUE calculation formulation
                                        frac = piru13C$frac) #Specify any post-photosynthetic fractionations. In this case 2 permille to account for leaf to wood.

#Create dataframe for visualizing differences in computed iWUE among the three formulations
piru13C_long <- piru13C %>%
  select(Year, Site, iWUE_simple, iWUE_photorespiration, iWUE_mesophyll) %>% #Select only columns of interest
  rename(Simple = iWUE_simple,
         Photorespiration = iWUE_photorespiration,
         Mesophyll = iWUE_mesophyll) %>% 
  pivot_longer(names_to = "Formulation", values_to = "iWUE", -c(Year, Site))

#Visually examine differences in iWUE based on the formulation used for each study location
ggplot(data = piru13C_long, aes(x = Year, y = iWUE, color = Formulation)) +
  geom_point(alpha = 0.5) +
  geom_smooth(aes(group = Formulation), color = "gray30") +
  theme_classic() +
  facet_wrap(~Site) +
  ylab(expression("iWUE (µmol mol"^{-1}*")")) 
#> `geom_smooth()` using method = 'loess' and formula 'y ~ x'
```

![](man/figures/README-piru13C-1.png)<!-- -->

## Literature cited

Badeck, F.-W., Tcherkez, G., Nogués, S., Piel, C. & Ghashghaie, J.
(2005). Post-photosynthetic fractionation of stable carbon isotopes
between plant organs—a widespread phenomenon. Rapid Commun. Mass
Spectrom., 19, 1381–1391.

Belmecheri, S. & Lavergne, A. (2020). Compiled records of atmospheric
CO<sub>2</sub> concentrations and stable carbon isotopes to reconstruct
climate and derive plant ecophysiological indices from tree rings.
Dendrochronologia, 63, 125748.

Bernacchi, C.J., Singsaas, E.L., Pimentel, C., Portis Jr, A.R. & Long,
S.P. (2001). Improved temperature response functions for models of
Rubisco-limited photosynthesis. Plant, Cell Environ., 24, 253–259.

Craig, H. (1953). The geochemistry of the stable carbon isotopes.
Geochim. Cosmochim. Acta, 3, 53–92.

Cernusak, L. A. & Ubierna, N. Carbon Isotope Effects in Relation to
CO<sub>2</sub> Assimilation by Tree Canopies. in Stable Isotopes in Tree
Rings: inferring physiological, climatic, and environmental responses
291–310 (2022).

Davies, J.A. & Allen, C.D. (1973). Equilibrium, Potential and Actual
Evaporation from Cropped Surfaces in Southern Ontario. J. Appl.
Meteorol., 12, 649–657.

Farquhar, G., O’Leary, M. & Berry, J. (1982). On the relationship
between carbon isotope discrimination and the intercellular carbon
dioxide concentration in leaves. Aust. J. Plant Physiol., 9, 121–137.

Frank, D.C., Poulter, B., Saurer, M., Esper, J., Huntingford, C., Helle,
G., et al. (2015). Water-use efficiency and transpiration across
European forests during the Anthropocene. Nat. Clim. Chang., 5, 579–583.

Gong, X. Y. et al. Overestimated gains in water‐use efficiency by global
forests. Glob. Chang. Biol. 1–12 (2022)

Lavergne, A. et al. Global decadal variability of plant carbon isotope
discrimination and its link to gross primary production. Glob. Chang.
Biol. 28, 524–541 (2022).

Ma, W. T. et al. Accounting for mesophyll conductance substantially
improves <sup>13</sup>C-based estimates of intrinsic water-use
efficiency. New Phytol. 229, 1326–1338 (2021).

Mathias, J. M. & Thomas, R. B. Disentangling the effects of acidic air
pollution, atmospheric CO2 , and climate change on recent growth of red
spruce trees in the Central Appalachian Mountains. Glob. Chang. Biol.
24, 3938–3953 (2018).

Tsilingiris, P.T. (2008). Thermophysical and transport properties of
humid air at temperature range between 0 and 100°C. Energy Convers.
Manag., 49, 1098–1110.

Ubierna, N. & Farquhar, G.D. (2014). Advances in measurements and models
of photosynthetic carbon isotope discrimination in C<sub>3</sub> plants.
Plant. Cell Environ., 37, 1494–1498.
