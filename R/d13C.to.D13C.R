#' @title d13C.to.D13C
#'
#' @description Calculates leaf carbon isotope discrimination given plant tissue d13C signature.
#'
#' @param d13C.plant Measured plant tissue carbon isotope signature, per mille (‰)
#' @param year Year to which the sample corresponds
#' @param frac Post-photosynthetic fractionation factor, defaults to 0 assuming leaf material with no post-photosynthetic fractionation. User should supply reasonable value if leaf fractionation present or if samples are from wood (generally -1.9 - -2.1).
#'
#' @return Carbon isotope discrimination in units of per mille (‰).
#'
#' @references
#' Badeck, F.-W., Tcherkez, G., Nogués, S., Piel, C. & Ghashghaie, J. (2005). Post-photosynthetic fractionation of stable carbon isotopes between plant organs—a widespread phenomenon. Rapid Commun. Mass Spectrom., 19, 1381–1391.
#'
#' Belmecheri, S. & Lavergne, A. (2020). Compiled records of atmospheric CO2 concentrations and stable carbon isotopes to reconstruct climate and derive plant ecophysiological indices from tree rings. Dendrochronologia, 63, 125748.
#'
#' Craig, H. (1953). The geochemistry of the stable carbon isotopes. Geochim. Cosmochim. Acta, 3, 53–92.
#'
#' Cernusak, L. A. & Ubierna, N. Carbon Isotope Effects in Relation to CO2 Assimilation by Tree Canopies. in Stable Isotopes in Tree Rings: inferring physiological, climatic, and environmental responses 291–310 (2022). doi:10.1007/978-3-030-92698-4_9.
#'
#' Farquhar, G., O’Leary, M. & Berry, J. (1982). On the relationship between carbon isotope discrimination and the intercellular carbon dioxide concentration in leaves. Aust. J. Plant Physiol., 9, 121–137.
#'
#' Frank, D.C., Poulter, B., Saurer, M., Esper, J., Huntingford, C., Helle, G., et al. (2015). Water-use efficiency and transpiration across European forests during the Anthropocene. Nat. Clim. Chang., 5, 579–583.
#'
#' Ubierna, N. & Farquhar, G.D. (2014). Advances in measurements and models of photosynthetic carbon isotope discrimination in C3 plants. Plant. Cell Environ., 37, 1494–1498.
#'
#' @export
#'
#' @examples
#' d13C.to.D13C(d13C.plant = -27, year = 2015)
#'
#'
#'
#'
d13C.to.D13C <- function(d13C.plant, year, frac = 0) {

  #Fractionation factor
  d <- frac

  #Assign d13C.atm based on year given.
  d13C.atm <- CO2data[which(CO2data$yr == year),3]
  Ca <- CO2data[which(CO2data$yr == year),2]
  D13C <- ((d13C.atm - (d13C.plant - d))/(1 + ((d13C.plant - d)/1000)))

  return(D13C)
}
