test_that("An empty list() results in empty `ptype` returned", {
  expect_equal(pslide(list(), ~.x), list())
  expect_equal(pslide_dbl(list(), ~.x), numeric())
  expect_equal(pslide_vec(list(), ~.x, .ptype = 1:5), integer())
})
