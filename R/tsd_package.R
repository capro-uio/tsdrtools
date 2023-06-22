#' Prepare R package for TSD install
#'
#' This function will download a package
#' with all its dependencies to a zipped archive,
#' to them be installed within TSD using
#' \code{\link{tsd_package_install}}.
#'
#' @template package
#' @template folder
#' @template repos
#' @template verbose
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
tsd_package_prepare <- function(package,
                                folder = NULL,
                                repos = getOption("repos"),
                                verbose = TRUE,
                                zip = TRUE,
                                ...) {
  k <- available.packages()
  pak <- grep(sprintf("^%s$", package), k[,1])
  if(length(pak) == 0)
    cli::cli_abort("Package not available, did you spell it correctly?")
  if(is.null(folder))
    folder <- sprintf("./%s_%s", package, k[pak, 2])
  if(!dir.exists(folder))
    dir.create(folder, recursive = TRUE, showWarnings = FALSE)
  folder <- normalizePath(folder)

  if(missing(package))
    cli::cli_abort("package to prepare is missing.")

  ## Get the list of packages to download, including any dependencies:
  if(verbose)
    cli::cli_alert("Finding all dependencies. Please wait...\n")
  pkgs <- pak::pkg_deps_tree(package)
  pkgs <- c(pkgs$package, package)
  pkgs <- pkgs[!pkgs %in% core_pkgs()]

  dl_paks <- pak::pkg_download(
    package,
    dest_dir = folder,
    dependencies = NA,
    r_versions = "4.4",
    platforms = c("source", "windows"))
  browser()

  install.packages("psych", repos = folder, type = "source")

  pfiles <- list.files(folder,
             "tar.gz$",
             full.names = TRUE,
             recursive = TRUE)

  k <- mapply(file.copy,
         from = pfiles,
         to = file.path(folder, basename(pfiles))
         )

  # do the workarounds
  lapply(pkgs[pkgs %in% names(except)],
         package_workarounds,
         folder = folder,
         type = "prepare",
         verbose = verbose)

  # Check that all packages are downloaded and in the correct folder
  d_pkgs <- list.files(folder, pattern = "tar.gz")

  pkg_check <-sapply(pkgs,
                     function(x) grep(sprintf("^%s_", x), d_pkgs))
  if(inherits(pkg_check, "list")){
    cli::cli_warn("The following dependencies have not been downloaded:")
    cli::cli_bullets(names(pkg_check[sapply(pkg_check, length) == 0]))
    cli::cli_alert("Please report an issue on GitHub for help:")
    cli::cli_alert_info("https://github.com/LCBC-UiO/tsdrtools/issues")
    unlink(folder, recursive = TRUE)
    cli::cli_abort("Exiting process.")
  }
  writeLines(pkgs,
             con = file.path(folder, "_pkg_install_order"),
             sep = "\n")
  all_files <- list.files(folder,
                          paste0(pkgs, collapse = "|"),
                          full.names = TRUE)
  if(zip){
    if(verbose)
      cli::cli_alert("zipping folder to prepare for TSD import")
    utils::zip(
      zipfile = paste0(gsub("/$", "", folder), ".zip"),
      files = all_files,
      flags = "-r9Xj")
    unlink(folder, recursive = TRUE)
  }

  if(verbose){
    cli::cli_alert_success("Package archive created.")
    cli::cli_alert("Import the package zip file to TSD (https://data.tsd.usit.no/) and continue with the tsd_install_package function.")
  }
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
#' @param opts optional arguments to `R CMD INSTALL`
#' @template verbose
#' @param ... additional arguments to functions \code{\link[utils]{install.packages}}
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
tsd_package_install <- function(zip_file,
                                lib = .libPaths()[1],
                                opts = "",
                                verbose = TRUE,
                                ...){
  stopifnot(file.exists(zip_file))
  # if folder is zipped, unzip
  folder <- gsub("\\.zip", "", zip_file)
  k <- utils::unzip(zip_file, exdir = folder)

  order <- readLines(file.path(folder, "_pkg_install_order"))
  pkgs <- list.files(folder,
                     pattern = "tar.gz$",
                     full.names = TRUE)
  pkgs <- pkgs[sapply(order, grep, x = pkgs)]
  paks <- do.call(rbind,
                  strsplit(gsub("\\.tar\\.gz",
                                "", order),
                           "_"))
  paks <- as.data.frame(paks)
  names(paks) <- c("pkg", "version")
  paks$paths <- normalizePath(pkgs)

  if(verbose)
    cli::cli_alert_info("Starting installations")

  j <- mapply(
    run_install,
    path = paks$path,
    pkg = paks$pkg,
    version = paks$version,
    MoreArgs = list(
      folder = folder,
      verbose = verbose,
      opts = opts,
      lib = lib
    ), SIMPLIFY = FALSE
  )
  names(j) <- paks$pkg
  tsdrtools_install(j)
}


