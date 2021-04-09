context("kobo_token")

test_that("token should be character", {
  skip_on_cran()
  skip_if_offline()
  output <- letters
  expect_type(output, "character")
})