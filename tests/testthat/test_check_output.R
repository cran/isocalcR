context("test_check_output")

#Test whether the d13C.to.iWUE output is numeric
test_that("d13C.to.iWUE() returns a numeric value", {
  iWUE <- d13C.to.iWUE(-26, 2015, 300, 26)
  expect_type(iWUE, "double")
})

#Test whether the d13C.to.D13C output is numeric
test_that("d13C.to.D13C() returns a numeric value", {
  D13C <- d13C.to.D13C(-26, 2015)
  expect_type(D13C, "double")
})

#Test whether the d13C.to.Ci output is numeric
test_that("d13C.to.Ci() returns a numeric value", {
  Ci <- d13C.to.Ci(-26, 2015, 300, 26)
  expect_type(Ci, "double")
})

#Test whether the d13C.to.CiCa output is numeric
test_that("d13C.to.CiCa() returns a numeric value", {
  CiCa <- d13C.to.CiCa(-26, 2015, 300, 26)
  expect_type(CiCa, "double")
})

#Test whether the d13C.to.CiCa output is between 0 and 1
test_that("d13C.to.CiCa() returns a numeric value", {
  CiCa <- d13C.to.CiCa(-26, 2015, 300, 26)
  expect_lt(CiCa, 1)
})

#Test whether the d13C.to.diffCaCia output is numeric
test_that("d13C.to.diffCaCi() returns a numeric value", {
  diffCaCi <- d13C.to.diffCaCi(-26, 2015, 300, 26)
  expect_type(diffCaCi, "double")
})

