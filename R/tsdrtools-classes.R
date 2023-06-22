# tsdrtools_install ----
#' Constructor for tsdrtools_install
#'
#' @param x list to with status, package, version and install log
#'
#' @export
tsdrtools_install <- function(x) {
  stopifnot(inherits(x, "list"))
  stopifnot(all(sapply(x, is_tsdrtools_status)))
  structure(
    do.call(rbind, x),
    class = c("tsdrtools_install",
              "tdl_df", "tbl",
              "data.frame")
  )
}

#' Validate tsdrtools_install
#' @param x an object
#' @export
is_tsdrtools_install <- function(x) inherits(x,'tsdrtools_install')


#' @export
format.tsdrtools_install <- function(x, ...) {
  fails <- x[!x$status, ]
  succs <- x[x$status,  ]
  class(x) <- class(x)[-1]
  c(
    sprintf("# Offline installation status"),
    sprintf("  Installation succeeded for %s packages: %s", nrow(succs), cli::ansi_collapse(succs$pkg)),
    sprintf("  Installation *failed* for %s packages: %s", nrow(fails), cli::ansi_collapse(fails$pkg)),
    "",
    capture.output(x, ...)
  )
}

#' @export
print.tsdrtools_install <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


#' @export
as.data.frame.tsdrtools_install <- function(x, ...){
  browser()
  x$status
}

# install status ----
tsd_install_status <- function(package,
                               version,
                               status,
                               log){
  dt <- data.frame(
    pkg = package,
    version = version,
    status = status
  )
  dt$log <- list(install_log(log))
  structure(
    dt,
    class = c("tsdrtools_status",
              "tbl_df", "tbl" ,
              "data.frame")
  )
}

is_tsdrtools_status <- function(x){
  inherits(x, "tsdrtools_status")
}

# install_log ----
#' Constructor for install_log
#'
#' @param x named list of installation logs
#'
#' @export
install_log <- function(x) {
  if(length(x) > 0) stopifnot(length(names(x)) > 0)
  structure(
    x,
    class = c("install_log", "character")
  )
}

#' Create install_log
#' @param x list to make install_log
#'
#' @export
as_install_log <- function(x) {
  install_log(x)
}

#' Validate install_log
#' @param x an object
#' @export
is_install_log <- function(x) inherits(x, 'install_log')

#' Validate install_error
#' @export
#' @inheritParams install_log
is.install_log <- install_log


#' @export
format.install_log <- function(x, ...) {
  sapply(x, paste0, collapse="\n")
}

#' @export
print.install_log <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


`[.install_log` <- function(x, i) {
  install_log(NextMethod())
}

#' Extract error logs from tsdrstools_install-object
#'
#' @param x object of class tsdrstools_install
#' @param package NULL or character vector of package errors to extract. If NULL extracts all.
#'
#' @return list of errors
#' @export
#'
#' @examples
#' \dontrun{
#' status <- tsd_package_install("devtools.zip")
#' error_logs(status)
#' }
error_logs <- function(x, package = NULL){
  stopifnot(inherits(x, "tsdrtools_install"))
  if(is.null(package)){
    package <- x[!x$status, "pkg"]
  }
  idx <- which(x$pkg %in% package)
  jj <- lapply(idx, function(j) x[j, "log"])
  names(jj) <- package
  jj
}
