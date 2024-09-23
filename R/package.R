#' Install Tobac
#'
#' `install_tobac()` installs the Python modules needed to run Tobac functions.
#' The packages are installed in a _virtualenv_, which by default is named
#' "harp-py". This virtual environment is the default virtual environment in
#' which all harp related Python modules are installed. If you already have the
#' "harp-py" virtualenv, you will be asked if you want to remove the virtualenv
#' prior to installation. Typically you would answer no to this.
#'
#' @param envname The virtual environment into which to install the tobac Python
#'   modules. Default is "harp-py".
#' @param new_env Whether to start from a new environment by removing the
#'   `envname` if it already exists. The defulat is TRUE if `envname` already
#'   exists, and you will be prompted if you want to remove the virtualenv
#'   `envname`.
#'
#' @export
#'
install_tobac <- function(
  envname = "harp-py",
  new_env = identical(envname, "harp-py")
) {

  if (new_env && reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_remove(envname)
  }

  if (!reticulate::virtualenv_exists(envname)) {
    reticulate::virtualenv_create(envname)
  }

  pip <- file.path(reticulate::virtualenv_root(), envname, "bin", "pip")

  message("Installing tobac to virtualenv: ", envname)
  system(
    paste(
      pip, "install --upgrade git+ssh://git@github.com/tobac-project/tobac.git"
    )
  )

  reticulate::py_install("Numba", envname)

}

.onLoad <- function(libname, pkgname) {
  reticulate::use_virtualenv("harp-py", required = FALSE)

  tobac <<- reticulate::import("tobac", delay_load = TRUE, convert = FALSE)

}

#' Main tobac module
#'
#' Interface to main Tobac module. Provides access to top level functions as
#' well as sub-modules using `$` instead of `.` as in Python.
#'
#' @format Tobac module
#'
#' @examples
#' \dontrun{
#' library(tobac)
#'
#' tobac$sys$api_version
#' }
#'
#' @export
toabc <- NULL
