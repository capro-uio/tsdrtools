

#' Prepare R package for TSD install
#'
#' THis function will download a package
#' with all its dependencies to a zipped archive,
#' to them be installed within TSD using
#' \code{\link{tsd_package_install}}.
#'
#' @param package name of package to install
#' @param folder folder to place all neccessary files in
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
    utils::zip(zipfile = paste0(folder, ".zip"), files = all_files)
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
#' @param verbose logical if status messages should be printed
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
tsd_package_install <- function(zip_file, verbose = TRUE, ...){

  # if folder is zipped, unzip
  utils::unzip(zip_file)
  folder <- gsub("\\.zip", "", zip_file)

  order <- readLines(file.path(folder, "pkg_install_order.list"))
  pkgs <- list.files(folder, pattern = "tar.gz", full.names = TRUE)
  pkgs <- pkgs[sapply(order, grep, x = pkgs)]

  j <- data.frame(pkg = pkgs,
                  success = NA)
  for(k in 1:length(pkgs)){
    j <- utils::install.packages(pkgs[k],
                               verbose = verbose,
                               repos = NULL, ...)
  }

  pkgs <- unlist(lapply(strsplit(order, "_"), function(x) x[1]))
  pkgs_i <- as.data.frame(installed.packages(), stringsAsFactors = FALSE)

  j <- data.frame(success = ifelse(pkgs %in% pkgs_i$Package, "success", "failed"),
                  pkg = paste0(pkgs, "\n"), stringsAsFactors = FALSE
  )

  k <- apply(j, 1, function(x) cat(x, sep="\t"))
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
