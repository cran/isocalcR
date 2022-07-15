#' @title custom.calc
#'
#' @description Calculates D13C, Ci, CiCa, diffCaCi, or iWUE with user specified values of d13C.plant, d13CO2.atm, atmospheric CO2, temperature, and elevation. The user can also specify whether to calculate each physiological index using the 'simple', 'photorespiration', or 'mesophyll' formulation in all calculations where Ci is computed. Method defaults to 'simple' assuming 'leaf' tissue and incorporates an apparent fractionation by Rubisco, b, of 27 permille (Cernusak and Ubierna 2022). If 'wood' tissue is supplied as an argument in the 'simple' method, the apparent fractionation by Rubisco, b, is updated to 25.5 permille (Cernusak and Ubierna 2022).
#'
#' @param d13C.plant Measured plant tissue carbon isotope signature, per mille (‰)
#' @param d13C.atm Atmospheric d13CO2, per mille (‰)
#' @param frac Post-photosynthetic fractionation factor, defaults to 0 assuming leaf material, user should supply reasonable value if from wood (generally -1.9 - -2.1)
#' @param outvar Variable of interest to calculate from the following: D13C, Ci, CiCa, diffCaCi, or iWUE. Defaults to D13C.
#' @param Ca Atmospheric CO2 concentration (ppm).
#' @param elevation Elevation (m.a.s.l.) of the sample, necessary to account for photorespiration processes
#' @param temp Leaf temperature (°C)
#' @param method Method to calculate iWUE (simple, photorespiration, or mesophyll). Defaults to 'simple'. See Lavergne et al. 2022, Ma et al. 2021, Gong et al. 2022.
#' @param tissue Plant tissue of the sample (i.e. leaf or wood) used only during calculations using the simple formulation. Defaults to 'leaf'.
#'
#' @return One of the specified outvars: D13C (permille), Ci (ppm), CiCa (unitless), diffCaCi (ppm), or iWUE (micromol CO2 per mol H2O). Defaults to 'D13C'.
#'
#' @references
#' Badeck, F.-W., Tcherkez, G., Nogués, S., Piel, C. & Ghashghaie, J. (2005). Post-photosynthetic fractionation of stable carbon isotopes between plant organs—a widespread phenomenon. Rapid Commun. Mass Spectrom., 19, 1381–1391.
#'
#' Belmecheri, S. & Lavergne, A. (2020). Compiled records of atmospheric CO2 concentrations and stable carbon isotopes to reconstruct climate and derive plant ecophysiological indices from tree rings. Dendrochronologia, 63, 125748.
#'
#' Bernacchi, C.J., Singsaas, E.L., Pimentel, C., Portis Jr, A.R. & Long, S.P. (2001). Improved temperature response functions for models of Rubisco-limited photosynthesis. Plant, Cell Environ., 24, 253–259.
#'
#' Craig, H. (1953). The geochemistry of the stable carbon isotopes. Geochim. Cosmochim. Acta, 3, 53–92.
#'
#' Cernusak, L. A. & Ubierna, N. Carbon Isotope Effects in Relation to CO2 Assimilation by Tree Canopies. in Stable Isotopes in Tree Rings: inferring physiological, climatic, and environmental responses 291–310 (2022). doi:10.1007/978-3-030-92698-4_9.
#'
#' Davies, J.A. & Allen, C.D. (1973). Equilibrium, Potential and Actual Evaporation from Cropped Surfaces in Southern Ontario. J. Appl. Meteorol., 12, 649–657.
#'
#' Farquhar, G., O’Leary, M. & Berry, J. (1982). On the relationship between carbon isotope discrimination and the intercellular carbon dioxide concentration in leaves. Aust. J. Plant Physiol., 9, 121–137.
#'
#' Frank, D.C., Poulter, B., Saurer, M., Esper, J., Huntingford, C., Helle, G., et al. (2015). Water-use efficiency and transpiration across European forests during the Anthropocene. Nat. Clim. Chang., 5, 579–583.
#'
#' Gong, X. Y. et al. Overestimated gains in water‐use efficiency by global forests. Glob. Chang. Biol. 1–12 (2022) doi:10.1111/gcb.16221.
#'
#' Lavergne, A. et al. Global decadal variability of plant carbon isotope discrimination and its link to gross primary production. Glob. Chang. Biol. 28, 524–541 (2022).
#'
#' Ma, W. T. et al. Accounting for mesophyll conductance substantially improves 13C-based estimates of intrinsic water-use efficiency. New Phytol. 229, 1326–1338 (2021).
#'
#' Tsilingiris, P.T. (2008). Thermophysical and transport properties of humid air at temperature range between 0 and 100°C. Energy Convers. Manag., 49, 1098–1110.
#'
#' Ubierna, N. & Farquhar, G.D. (2014). Advances in measurements and models of photosynthetic carbon isotope discrimination in C3 plants. Plant. Cell Environ., 37, 1494–1498.
#'
#' @export
#'
#' @examples
#' custom.calc(d13C.plant = -27, d13C.atm = -8.7)
#'
#'
#'
#'
custom.calc <- function(d13C.plant,
                        d13C.atm,
                        frac = 0,
                        outvar = "D13C",
                        Ca = NULL,
                        elevation = NULL,
                        temp = NULL,
                        method = "simple",
                        tissue = "leaf") {

  #Define constants
  a <- 4.4 #Fractionation associated with diffusion, Craig 1953.
  am <- 1.8 #Fractionation during liquid diffusion and dissolution of CO2 in mesophyll (0.7 + 1.1).
  gscovergm <- 0.79 #Ratio of stomatal conductance to CO2 and mesophyll conductance, Ma et al. 2021.
  b <- switch(method,
              "simple" = switch(tissue, "leaf" = 27, "wood" = 25.5), #Fractionation associated with effective Rubisco carboxylation, Cernusak and Ubierna 2022.
              "photorespiration" = 28, #Lavergne et al 2022
              "mesophyll" = 29) #Ma et al. 2021
  d <- ifelse(outvar == "D13C", #If outvar is D13C, must allow frac to be user supplied, otherwise frac depends on "method"
              frac,
              switch(method,
                     "simple" = 0, #No fractionation in "simple", Cernusak and Ubierna 2022.
                     "photorespiration" = frac, #1.9 for bulk wood, Badeck et al. 2005, 2.1 for a-cellulose, Frank et al. 2015.
                     "mesophyll" = frac)) #1.9 for bulk wood, Badeck et al. 2005, 2.1 for a-cellulose, Frank et al. 2015.)

  D13C <- ((d13C.atm - (d13C.plant - d))/(1 + ((d13C.plant - d)/1000)))

  f <- 12 #Fractionation associated with photorespiration, Ubierna and Farquhar 2014.
  #Calculate atmospheric pressure (Pa), given elevation.
  P0 <- 101325
  Base.temp <- 298.15    #Base temperature, units (K)
  ALR  <- 0.0065    #Adiabiatic lapse rate, units (K/m), Davies and Allen 1973
  Grav  <- 9.80665   #Gravitational acceleration, units (m/s^2), Davies and Allen 1973
  R  <- 8.3145    #Universal gas constant, units (J/mol/K), Davies and Allen 1973
  MWair <- 0.028963  #Molecular weight of dry air, units (kg/mol), Tsilingiris 2008
  Patm <- P0*(1.0 - ALR*elevation/Base.temp)^(Grav*MWair/(R*ALR)) #Finally, convert elevation to pressure, Pa.
  deltaHa <- 37830 #Activation energy for Gammastar (J/mol), Bernacchi et al. 2001.
  Temp.C <- temp
  Gammastar25 <- 4.332 #Pa, value based on Bernacchi et al. (2001), converted to Pa by T. Davis assuming elevation of 227.076 m.a.s.l. From Beni Stocker's RPmodel.
  Gammastar <- Gammastar25*Patm/P0*exp(1)^((deltaHa*((Temp.C+273.15)-298.15))/(R*(Temp.C+273.15)*298.15)) #CO2 compensation point in the absence of mitochondrial respiration, units (Pa)

  pCa <- (1.0e-6)*Ca*Patm #Need to convert atm CO2 (ppm) to atm CO2 (Pa)
  Ci <- ((D13C-a+f*(Gammastar/pCa))/(b-a))*Ca
  Ci <- switch (method,
                "simple" =  (D13C -a)/(b-a)*Ca,  #Laverge et al 2022
                "photorespiration" = ((D13C-a+f*(Gammastar/pCa))/(b-a))*Ca, #Laverge et al 2022
                "mesophyll" = -(Ca*((b-D13C-f*(Gammastar/pCa))/(b-a+(gscovergm*(b-am))))-Ca)) #Ma et al. 2021
  CiCa <- Ci/Ca
  diffCaCi <- Ca - Ci
  iWUE <- (Ca - Ci)*0.625

  out <- switch (outvar,
                 "D13C" = D13C,
                 "Ci" = Ci,
                 "CiCa" = CiCa,
                 "diffCaCi" = diffCaCi,
                 "iWUE" = iWUE)

  return(out)

}
