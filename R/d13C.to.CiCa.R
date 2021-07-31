#' @title d13C.to.CiCa
#'
#' @description Calculates the ratio of the concentration of leaf intercellular to atmospheric CO2, unitless.
#'
#' @param d13C Measured plant tissue carbon isotope signature, per mille (‰)
#' @param year Year to which the sample corresponds
#' @param elevation Elevation (m.a.s.l.) of the sample, necessary to account for photorespiration processes
#' @param temp Leaf temperature (°C)
#' @param frac Post-photosynthetic fractionation factor, defaults to 0 assuming leaf material, user should supply reasonable value if from wood (generally -1.9 - -2.1)
#'
#' @return The ratio of leaf intercellular to atmospheric CO2 (Ci/Ca), unitless
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
#' Davies, J.A. & Allen, C.D. (1973). Equilibrium, Potential and Actual Evaporation from Cropped Surfaces in Southern Ontario. J. Appl. Meteorol., 12, 649–657.
#'
#' Farquhar, G., O’Leary, M. & Berry, J. (1982). On the relationship between carbon isotope discrimination and the intercellular carbon dioxide concentration in leaves. Aust. J. Plant Physiol., 9, 121–137.
#'
#' Frank, D.C., Poulter, B., Saurer, M., Esper, J., Huntingford, C., Helle, G., et al. (2015). Water-use efficiency and transpiration across European forests during the Anthropocene. Nat. Clim. Chang., 5, 579–583.
#'
#' Tsilingiris, P.T. (2008). Thermophysical and transport properties of humid air at temperature range between 0 and 100°C. Energy Convers. Manag., 49, 1098–1110.
#'
#' Ubierna, N. & Farquhar, G.D. (2014). Advances in measurements and models of photosynthetic carbon isotope discrimination in C3 plants. Plant. Cell Environ., 37, 1494–1498.
#'
#' @export
#'
#' @examples
#' d13C.to.CiCa(-27, 2015, 900, 24)
#'
#'
#'
#'
d13C.to.CiCa<- function(d13C, year, elevation, temp, frac = 0) {

  #Assign d13C as d13C.plant
  d13C.plant <- d13C
  #Assign d13C.atm based on year given.
  d13C.atm <- CO2data[which(CO2data$yr == year),3]
  Ca <- CO2data[which(CO2data$yr == year),2]
  a <- 4.4 #Fractionation associated with diffusion, Craig 1953.
  b <- 28 #Fractionation associated with Rubisco carboxylation, Ubierna and Farquhar 2014.
  d <- frac #1.9 for bulk wood, Badeck et al. 2005, 2.1 for a-cellulose, Frank et al. 2015.
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
  CiCa <- Ci/Ca

  return(CiCa)
}
