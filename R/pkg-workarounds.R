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

stringi_workaround_prepare <- function(folder, verbose = TRUE){
  icudt61l <- "http://www.ibspan.waw.pl/~gagolews/stringi/icudt61l.zip"
  utils::download.file(icudt61l, file.path(folder, "icudt61l.zip"), quiet = !verbose)
}

stringi_workaround_install <- function(folder){
  folder <- normalizePath(folder)
  paste0("--configure-vars=\"ICUDT_DIR=", folder, "\"")
}

package_workarounds_install <- function(package, folder){
  if(grepl("stringi", package))
    return(stringi_workaround_install(folder))
}


