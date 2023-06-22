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
                                type = c("Depends", "Imports"),
                                repos = getOption("repos"),
                                verbose = TRUE,
                                ...,
                                i = 1,
                                spinner = NULL
) {
  if(is.null(spinner)){
    spinner <- cli::make_spinner("simpleDotsScrolling", template = "Checking dependency tree {spin}")
  }else{
    spinner$spin()
  }
  type <- match.arg(type,
                    c("Depends", "Imports", "LinkingTo", "Suggests", "Enhances"),
                    several.ok = TRUE)
  i <- i + 1
  packages <- unlist(unname(
    package_dependencies(package,
                         available.packages(
                           repos = repos,
                           ...),
                         which = type)
  ))
  packages <- unique(packages)
  pack_to_run <- packages[!packages %in% c(package, core_pkgs())]

  if(length(pack_to_run) == 0){
    return(character())
  }

  paks <- sapply(
    pack_to_run,
    get_dependency_tree,
    i = i,
    type = type,
    repos = repos,
    verbose = verbose,
    spinner = spinner,
    ..., simplify = TRUE)

  if(i == 2){
    pack_to_run <- list(pack_to_run)
    names(pack_to_run) <- package
    paks <- c(pack_to_run, paks)
    pkgs <- data.frame(
      stringsAsFactors = FALSE,
      name = names(paks),
      deps = I(unname(paks))
    )
    spinner$finish()
    cat(cli::tree(pkgs, trim = FALSE),
        sep="\n")
  }
  invisible(names(paks))
}



check_installed <- function(x){
  if(grepl("DONE", x[length(x)])) return(TRUE)
  return(FALSE)
}

check_exceptions <- function(type){
  type <- match.arg(type,
                    c("prepare", "install"))
  sapply(workarounds, function(x) type %in% x)
}



run_install <- function(pkg, version, path, opts, folder, verbose, lib) {
  # Create log-file
  log_file <- paste0(
    pkg,
    format(Sys.time(), "%Y%m%d-%H%M%S.txt")
  )
  file.create(log_file)

  # Append options
  opts2 <- paste(
    opts,
    package_workarounds(pkg,
                        folder,
                        type = "install",
                        verbose = verbose)
  )

  cmd <- paste("CMD INSTALL",
               paste0("--library=", lib),
               opts2, path)

  # Run system install command
  system2("R", cmd ,
          stdout = log_file,
          stderr = log_file,
          stdin = log_file)

  # Grab log information and delete file
  log <- c(cmd, readLines(log_file))
  names(log) <- pkg

  invisible(file.remove(log_file))

  # check install status
  check <- check_installed(log)
  if(verbose){
    alert <- switch(
      ifelse(check, "success", "failed"),
      "success" = cli::cli_alert_success,
      "failed"  = cli::cli_alert_danger
    )
    alert(sprintf("%s\t%s", version, pkg))
  }
  return(tsd_install_status(
    status = check,
    package = pkg,
    version = version,
    log = log
  ))
}
