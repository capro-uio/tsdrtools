wrkdir <- testthat::test_path("testfldr")
zip_file <- paste0(wrkdir, ".zip")
package <- "DT"

test_that("tsd_package_prepare works", {
  expect_error(tsd_package_prepare(), "prepare is missing")

  expect_output(tsd_package_prepare(package, folder = wrkdir),
                "Finding all dependencies.")

  ffs <- list.files(wrkdir)
  expect_true("pkg_install_order.list" %in% ffs)
  order <- readLines(file.path(wrkdir, "pkg_install_order.list"))

  expect_true(any(grepl(package, order[length(order)])))

  # Remove test folder
  j <- unlink(wrkdir, recursive = TRUE)
  expect_true(j == 0)

})



test_that("tsd_package_install works", {

  expect_false(any(grepl(wrkdir, list.dirs(test_path()))))

  j <- tsd_package_install(zip_file, verbose = FALSE)
  expect_false(any("failed" %in% j$success))

  pkgs <- readLines(file.path(wrkdir, "pkg_install_order.list"))
  pkgs <- strsplit(gsub("\\.tar\\.gz", "", pkgs), "_")
  pkgs <- data.frame(Package = sapply(pkgs, function(x) x[1]),
                     Version_new = sapply(pkgs, function(x) x[2]))

  k <- as.data.frame(installed.packages())
  k <- k[k$Package %in% pkgs$Package, c("Package", "Version")]

  k <- merge(k, pkgs)
  k$check <- k$Version == k$Version_new

  expect_true(all(k$check))

  # Remove test folder
  unlink(c(wrkdir,zip_file), recursive = TRUE)
})



