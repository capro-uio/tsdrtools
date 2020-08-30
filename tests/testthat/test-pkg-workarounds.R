
test_that("core package list works", {
  pkgs <- c("base",
            "compiler",
            "datasets",
            "graphics",
            "grDevices",
            "grid",
            "methods",
            "parallel",
            "splines",
            "translations",
            "utils",
            "stats",
            "stats4",
            "tcltk",
            "tools")

  expect_true(all(core_pkgs() %in% pkgs))

})
