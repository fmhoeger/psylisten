context("test-utils.R")

test_that("psylisten packages returns character vector of package names", {
  out <- psylisten_packages()
  expect_type(out, "character")
  expect_true("pmcharrison/cabat" %in% out)
  expect_true("klausfrieler/EDT" %in% out)
  expect_true("pmcharrison/mdt" %in% out)
  expect_true("pmcharrison/mpt" %in% out)
  expect_true("pmcharrison/piat" %in% out)
  expect_true("klausfrieler/RAT" %in% out)
})
