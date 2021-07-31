#' @title CO2data
#'
#' @description Compiled records of atmospheric CO2 concentrations and stable carbon isotopes to reconstruct climate and derive plant ecophysiological indices from tree rings. Data are from Belmecheri, Lavergne, 2020, Dendrochronologia. Updated based on their methodology beyond C.E. 2019.
#'
#' @docType data
#'
#' @usage data(CO2data)
#'
#' @format A data frame with 2020 rows and 3 variables:
#' \describe{
#'   \item{yr}{Year of CO2 and d13CO2 measurement}
#'   \item{Ca}{Atmospheric CO2 concentration, in ppm}
#'   \item{d13C.atm}{Atmospheric d13CO2, in per mille, ‰}
#' }
#'
#' @keywords datasets
#'
#' @references Belmecheri, S. & Lavergne, A. (2020). Compiled records of atmospheric
#' CO2 concentrations and stable carbon isotopes to reconstruct climate and derive
#' plant ecophysiological indices from tree rings. Dendrochronologia, 63, 125748.
#'
#' @source https://www.sciencedirect.com/science/article/abs/pii/S1125786520300874
#'
#' @examples
#'
#' data(CO2data)
#' head(CO2data)
#'
"CO2data"
