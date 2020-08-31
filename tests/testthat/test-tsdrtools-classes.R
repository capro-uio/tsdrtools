test_that("tsdrtools_install-class works", {

  expect_error(tsdrtools_install())

  expect_error(tsdrtools_install(success = list()))

  expect_error(tsdrtools_install(success = list(),
                                 error_logs = list()))

  expect_error(tsdrtools_install(success = data.frame(),
                                 error_logs = list()))

  success <- data.frame(pkg = c("clipr", "lme4"),
                        success = c("success", "failed"),
                        version = c("1.0", "2.3"),
                        stringsAsFactors = FALSE)

  j <- tsdrtools_install(success = success,
                         error_logs = list())

  expect_equal(class(j), "tsdrtools_install")

  expect_true(is_tsdrtools_install(j))
  expect_false(is_tsdrtools_install(as.list(j)))

})

test_that("tsdrtools_status-class works", {

  expect_error(tsdrtools_status())


  expect_error(tsdrtools_status(success = data.frame()))

  success <- data.frame(pkg = c("clipr", "lme4"),
                        success = c("success", "failed"),
                        version = c("1.0", "2.3"),
                        stringsAsFactors = FALSE)

  k <- as_tsdrtools_status(success)
  expect_true(is_tsdrtools_status(k))

})

test_that("install_error-class works", {

  expect_error(install_error())

  err_lo <- list(pkg1 = c("some", "error"),
                pkg2 = c("another", "error", "to", "catch"))

  kk <- as_install_error(err_lo)

  expect_true(is_install_error(kk))
})


test_that("error_logs-class works", {

  success <- data.frame(pkg = c("clipr", "lme4"),
                        success = c("success", "failed"),
                        version = c("1.0", "2.3"),
                        stringsAsFactors = FALSE)

  j <- tsdrtools_install(success = success,
                         error_logs = list(lme4 = c("some", "error")))

  expect_output(error_logs(j), "lme4")
  expect_output(error_logs(j, "lme4"), "lme4")
  expect_error(error_logs(j, "clipr"))
})
