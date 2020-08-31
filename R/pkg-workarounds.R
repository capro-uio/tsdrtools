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

  if(any(grepl("stringi", package)))
    return(stringi_workaround(folder, type, verbose))
}

#' Function for stringi workaround
#'
#' stringi requires download of a bundle
#' to work. If we want to install offline,
#' the bundle needs to be downloaded and
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
stringi_workaround <- function(folder,
                               type,
                               verbose = TRUE
){

  type <- match.arg(type,
                    c("prepare", "install"))

  if(type == "prepare"){
    icudt61l <- "http://www.ibspan.waw.pl/~gagolews/stringi/icudt61l.zip"
    utils::download.file(icudt61l,
                         file.path(folder, "icudt61l.zip"),
                         quiet = !verbose)
    return()
  }else{
    folder <- normalizePath(folder)
    x <- paste0("--configure-vars=\"ICUDT_DIR=", folder, "\"")
    return(x)
  }
}
