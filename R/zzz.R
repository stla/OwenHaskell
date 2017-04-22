.onLoad <- function(libname, pkgname) {
  library.dynam("Owen", pkgname, libname, now=TRUE)
  .C("HsStart")
  invisible()
}

.onUnLoad <- function(libpath) {
  library.dynam.unload("Owen", libpath)
  invisible()
}
