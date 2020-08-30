test_that("check installed function", {
  expect_true(check_installed(list(c("something", "something DONE"))))
  expect_false(check_installed(list(c("something", "something"))))
})


test_that("get_dependency_tree function", {
  expect_output(k <- get_dependency_tree("clipr", repos = "https://cloud.r-project.org/"), "clipr")
  expect_equal(k, "utils")
})
