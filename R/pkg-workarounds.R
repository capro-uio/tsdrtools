#' R core packages
#'
#' @return character vector of R core-packages
#' @export
core_pkgs <- function(){
  c("base",
    "compiler",
    "datasets",
    "graphics",
    "grDevices",
    "grid",
    "methods",
    "parallel",
    "splines",
    "stats",
    "stats4",
    "tcltk",
    "tools",
    "translations",
    "utils")
}


#' Retrieve package workaround
#'
#' Certain R-packages require workarounds
#' to install on offline servers.
#' This function will check if the given package
#' has such a work-around and return said
#' workaround for the type of process asked
#' (prepare or install).
#'
#' @template package
#' @template folder
#' @param type character vector of either "prepare" or "install".
#' This indicates if the workaround for install-process or
#' prepare-process should be returned.
#' @template verbose
package_workarounds <- function(package,
                                folder,
                                type,
                                verbose = TRUE){
  type <- match.arg(type,
                    c("prepare", "install"))
  if(!check_exceptions(type))
    return()
  func <- eval(parse(text = sprintf("%s_workaround", package)))
  func(folder, type, verbose)
}

workarounds <- c(
  "stringi" = "prepare"
)

#' Function for stringi workaround
#'
#' stringi requires download of a bundle
#' to work. If we want to install offline,
#' the bundle needs to be downloaded@ and
#' ported with the package tar.
#' For \code{type = "prepare"} the bundle is
#' downloaded into the folder with the pacakge
#' tars.
#' For install, the call to \code{R CMD INSTALL}
#' is given the path to the bundle during installation,
#' which will stop it from attempting to
#' download the bundle during install.
#'
#' @inheritParams package_workarounds
#' @noRd
stringi_workaround <- function(folder,
                               type,
                               verbose = TRUE
){

  type <- match.arg(type,
                    workarounds["stringi"])

  tmpf <- tempfile("stringi", fileext = ".zip")
  k <- download.file(
    "https://github.com/gagolews/stringi/archive/master.zip",
    tmpf)
  k <- utils::unzip(tmpf,
                    exdir = folder,
                    overwrite = TRUE)
  invisible(lapply(k, Sys.chmod, mode = "775"))
  Sys.chmod(folder, mode = "775", use_umask = FALSE)
  rbig <- readLines(k[1])
  rbig <- rbig[!grepl("icu..\\/data", rbig)]
  writeLines(rbig, k[1], sep = "\n")
  system(sprintf("cd %s; R CMD build %s", folder, "stringi-master"))
  unlink(file.path(folder, "stringi-master"), recursive = TRUE)
  return()
}
