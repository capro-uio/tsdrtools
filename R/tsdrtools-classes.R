# tsdrtools_install ----
#' Constructor for tsdrtools_install
#'
#' @param success data.frame of class \code{tsdrtools_status}
#' @param error_logs list of class \code{install_error}
#'
#' @export
tsdrtools_install <- function(success, error_logs) {

  stopifnot(is.list(error_logs))

  structure(
    list(
      success = as_tsdrtools_status(success),
      error_logs = as_install_error(error_logs)
    ),
    class = 'tsdrtools_install'
  )
}

#' Create tsdrtools_install
#' @param x list to make tsdrtools_install
#'
#' @export
as_tsdrtools_install <- function(x) {
  if(!is.null(names(x)) &
     all(names(x) %in% c("success", "error_logs"))){
    tsdrtools_install(
      as_tsdrtools_status(x$success),
      as_install_error(x$error_logs)
    )
  }else{
    stop("Cannot make object to tsdrtools_install")
  }
}

#' Validate tsdrtools_install
#' @param x an object
#' @export
is_tsdrtools_install <- function(x) inherits(x,'tsdrtools_install')


#' @export
format.tsdrtools_install <- function(x, ...) {
  fails <- x$success[match("failed", x$success$success),]
  succs <- x$success[match("success", x$success$success),]

  c(
    sprintf("# Offline installation status\n"),
    sprintf("# Installation succeeded for %s packages: %s", nrow(succs), paste0(succs$pkg, collapse = ", ")),
    sprintf("# Installation *failed* for %s packages: %s", nrow(fails), paste0(fails$pkg, collapse = ", "))
  )
}

#' @export
print.tsdrtools_install <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


#' @export
as.data.frame.tsdrtools_install <- function(x, ...){
  x$success
}

#' @export
as.list.tsdrtools_install <- function(x, ...){
  list(
    x$success,
    x$error_logs
  )
}

# tsdrtools_status ----
#' Constructor for tsdrtools_status
#'
#' @param success character vector of "success" or "failed"
#' @param pkg character vector of packages
#' @param version character vector of versions
#'
#' @export
tsdrtools_status <- function(success, pkg, version) {
  structure(
    data.frame(
      success = success,
      pkg = pkg,
      version = version,
      stringsAsFactors = FALSE
    ),
    class = c('tsdrtools_status', 'data.frame')
    )
}

#' Create tsdrtools_status
#' @param x list to make tsdrtools_status
#'
#' @export
as_tsdrtools_status <- function(x) {
  if(!is.null(names(x)) &
     all(c("success", "pkg", "version") %in% names(x))){
    tsdrtools_status(x$success, x$pkg, x$version)
  }else{
    stop("Cannot make object to tsdrtools_status")
  }
}

#' Validate tsdrtools_status
#' @param x an object
#' @export
is_tsdrtools_status <- function(x) inherits(x,'tsdrtools_status')

#' Validate tsdrtools_status
#' @export
#' @inheritParams is_tsdrtools_status
is.tsdrtools_status <- function(x) is_tsdrtools_status(x)

# install_error ----
#' Constructor for install_error
#'
#' @param x named list of installation errors
#'
#' @export
install_error <- function(x) {
  if(length(x) > 0) stopifnot(length(names(x)) > 0)
  structure(
    x,
    class = c('install_error')
  )
}

#' Create install_error
#' @param x list to make install_error
#'
#' @export
as_install_error <- function(x) {
  install_error(x)
}

#' Validate install_error
#' @param x an object
#' @export
is_install_error <- function(x) inherits(x, 'install_error')

#' Validate install_error
#' @export
#' @inheritParams is_install_error
is.install_error <- function(x) is_install_error(x)


#' @export
format.install_error <- function(x, ...) {
  sapply(x, paste0, collapse="\n")
}

#' @export
print.install_error <- function(x, ...) {
  cat(format(x), sep = "\n")
  invisible(x)
}


`[.install_error` <- function(x, i) {
  install_error(NextMethod())
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
  stopifnot(class(x) == "tsdrtools_install")

  if(is.null(package)) package <- names(x$error_logs)
  stopifnot(package %in% names(x$error_logs))

  idx <- match(package, names(x$error_logs))

  jj <- sapply(idx,
               function(i) paste(paste0("####\t", names(x$error_logs)[i]),
                                 paste0(x$error_logs[[i]], collapse="\n"), sep="\n")
  )

  k <- sapply(jj, cat, sep = "\n")

  ret <- lapply(idx, function(i) x$error_logs[i])

  invisible(ret)
}
