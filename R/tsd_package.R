

#' Prepare R package for TSD install
#'
#' THis function will download a package
#' with all its dependencies to a zipped archive,
#' to them be installed within TSD using
#' \code{\link{tsd_package_install}}.
#'
#' @param package name of package to install
#' @param folder folder to place all necessary files in
#' @param repos CRAN mirror to download from
#' @param verbose logical if status messages should be printed
#' @param zip logical, if folder should be zipped at the end
#' @param ... additional arguments to functions \code{\link[utils]{install.packages}}
#' and \code{\link[utils]{available.packages}}
#'
#' @return returns nothing, but creates a folder and zip archive
#' @export
#' @importFrom utils available.packages zip download.packages
#' @importFrom tools package_dependencies
#' @examples
#' \dontrun{
#' tsd_package_prepare("devtools")
#'
#' tsd_package_prepare("dplyr", folder = "dplyr_pkg")
#'
#' tsd_package_prepare("dplyr", repos = "https://cran.uib.no/", verbose = FALSE)
#'
#' }
tsd_package_prepare <- function(package, folder = package, repos = "https://cran.rstudio.com/",
                                verbose = TRUE, zip = TRUE, ...) {

  if(missing(package)) stop("package to prepare is missing.")

  ## Get the list of packages to download, including any dependencies:
  if(verbose) cat("Finding all dependencies.  Please wait...\n")
  pkgs <- tools::package_dependencies(package,
                                      db = available.packages(repos = repos, ...),
                                      verbose = verbose)[[1]]
  pkgs <- c(pkgs, package)
  pkgs <- pkgs[!pkgs %in% core_pkgs()]

  if(!dir.exists(folder)) dir.create(folder, recursive = TRUE)

  ## Download the packages, saving their order
  if(verbose) cat("Downloading packages:\n")
  l_pkgs <- utils::download.packages(pkgs, folder, quiet = !verbose, repos = repos, ...)[,2]

  # Check that all packages are downloaded and in the correct folder
  d_pkgs <- list.files(folder, pattern = "tar.gz")

  pkg_check <- basename(l_pkgs) %in% d_pkgs

  if(any(!pkg_check)){
    cat("The following dependencies have not been downloaded:\n")
    cat(basename(l_pkgs[!pkg_check]))
  }

  l_pkgs <- gsub(paste0(folder, "/"), "", l_pkgs)
  writeLines(l_pkgs,
             con = file.path(folder, "pkg_install_order.list"))

  if(zip){
    if(verbose) cat("zipping folder to prepare for TSD import\n")
    all_files <- list.files(folder, recursive = TRUE, full.names = TRUE)
    utils::zip(zipfile = paste0(gsub("/$", "", folder), ".zip"), files = all_files, flags = "-r9Xj")
  }

  if(verbose) cat("\nPackage archive created.",
                  "Import the package zip file to TSD (https://data.tsd.usit.no/)",
                  "and continue with the tsd_install_package function.\n")

}


#' Install package on TSD from archive
#'
#' After downloading and preparing the
#' packages for install outside of TSD
#' using the \code{\link{tsd_package_prepare}}
#' function, install said packages using
#' this function.
#'
#'
#' @param zip_file path to zipped file
#' @param lib library folder to install to
#' @param ... additional arguments to functions \code{\link[utils]{install.packages}}
#' and \code{\link[utils]{available.packages}}
#'
#' @return vector of successful or failed package installs
#' @export
#' @importFrom utils install.packages unzip installed.packages
#'
#' @examples
#' \dontrun{
#' # prepare a package first
#' tsd_package_prepare("devtools")
#'
#' tsd_package_install("devtools.zip")
#' }
tsd_package_install <- function(zip_file, lib = .libPaths()[1], ...){
  stopifnot(file.exists(zip_file))

  # if folder is zipped, unzip
  folder <- gsub("\\.zip", "", zip_file)
  k <- utils::unzip(zip_file, exdir = folder)

  order <- readLines(file.path(folder, "pkg_install_order.list"))
  pkgs <- list.files(folder, pattern = "tar.gz", full.names = TRUE)
  pkgs <- pkgs[sapply(order, grep, x = pkgs)]

  j <- do.call(rbind, strsplit(gsub("\\.tar\\.gz", "", order), "_"))
  j <- as.data.frame(j, stringsAsFactors = FALSE)
  names(j) <- c("pkg", "version")

  ff <- list()

  for(k in 1:length(pkgs)){
    ff[[k]] <- utils::capture.output(type = "output",
      install.packages(pkgs[k],
                       repos = NULL,
                       lib = lib,
                       configure.vars = stringi_workaround_install(pkgs[k], folder)
      )
    )
  }

  ff2 <- unlist(lapply(ff, check_installed))
  j$success <- ifelse(ff2, "success", "failed")

  j <- j[,c("success", "pkg", "version")]

  k <- apply(j, 1, function(x){
    cat(x, sep="\t")
    cat("\n")
  })
  invisible(j)
}

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
