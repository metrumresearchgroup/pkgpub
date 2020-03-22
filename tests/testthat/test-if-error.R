test_that("if error works", {
  attempt <- tryCatch(stop('ahh'), error = function(e) e)
  expect_null(pkgpub:::if_error(attempt))
  expect_equal(pkgpub:::if_error(attempt, "recovered"), "recovered")
})
