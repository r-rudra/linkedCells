test_that("generate_token returns valid unique tokens", {
  t1 <- generate_token()
  t2 <- generate_token()

  expect_type(t1, "character")
  expect_true(nchar(t1) > 10)
  expect_false(is.na(t1))
  expect_false(identical(t1, t2))
})



test_that("async works under multisession (local only)", {

  testthat::skip_on_ci()
  testthat::skip_on_cran()

  old_plan <- future::plan()
  on.exit(future::plan(old_plan), add = TRUE)

  future::plan(future::multisession)

  result <- NULL

  p <- with_async_safety(
    {
      Sys.sleep(0.1)
      list(data = 42, status = "success")
    },
    session = NULL
  )

  promises::then(p, function(res) {
    result <<- res
  })

  # Wait loop
  for (i in 1:100) {
    if (!is.null(result)) break
    Sys.sleep(0.01)
    later::run_now()
  }

  testthat::expect_equal(result$data, 42)
})
