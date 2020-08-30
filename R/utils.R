## function getDependencyTree()
## from https://gist.github.com/johnrc/faaa796e4b1ac53b7848
# Only parameter that's required is the package you want a dependency tree for
get_dependency_tree <- function(pack,
                                i = 1,
                                type = c("Depends", "Imports"),
                                repos = getOption("repos"),
                                verbose = TRUE,
                                ...
) {
  if(i == 1) if(verbose) cat(pack, "\n")
  i <- i + 1
  packages <- unlist(
    tools::package_dependencies(pack,
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
