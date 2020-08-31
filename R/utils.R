## from https://gist.github.com/johnrc/faaa796e4b1ac53b7848
#' Get package dependency tree
#'
#' This function will return all package dependencies
#' for any given package, including backwards dependency
#' for all.
#' The function mainly prints out the dependency tree
#' in the console, but also returns all packages
#' required for build (uniquely).
#'
#' @template package
#' @param i integer. Used in recursive call to package dependency retrieval.
#' @param type Character vector, a subset of c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances")
#' @template repos
#' @template verbose
#' @param ... additional arguments to functions \code{\link[utils]{available.packages}}
#' @importFrom utils available.packages
#' @importFrom tools package_dependencies
#' @return character vector of unique packages
#' @export
get_dependency_tree <- function(package,
                                i = 1,
                                type = c("Depends", "Imports"),
                                repos = getOption("repos"),
                                verbose = TRUE,
                                ...
) {

  type <- match.arg(type,
                    c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
                    several.ok = TRUE)

  if(i == 1) if(verbose) cat(package, "\n")
  i <- i + 1
  packages <- unlist(
    package_dependencies(package,
                         available.packages(
                           repos = repos,
                           ...),
                         which = type))
  pp <- packages

  for(pkg in packages) {
    for(n in 1:i) {
      if(verbose) cat(" ")
    }
    if(verbose) cat("|", pkg, "\n")
    pp <- c(pp,
            get_dependency_tree(pkg,
                                i,
                                type,
                                repos = repos,
                                verbose, ...)
    )
  }

  invisible(unique(unname(pp)))
}

check_installed <- function(x){
  if(grepl("DONE", x[length(x)])) return(TRUE)
  return(FALSE)
}
