# R/zzz.R

# Create a package-local environment to store the previous state
pkg_env <- new.env(parent = emptyenv())

.onLoad <- function(libname, pkgname) {
  # Capture the current state of the arrow option before modifying
  pkg_env$old_arrow_opts <- options(arrow.pull_as_vector = TRUE)
}

.onUnload <- function(libpath) {
  # Restore the user's original option when the package is detached
  options(pkg_env$old_arrow_opts)
}
