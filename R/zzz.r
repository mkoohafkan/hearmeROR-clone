#' @importFrom lubridate origin
.onLoad = function(libname, pkgname) {
  default.stage.template <<- system.file("stage.template.yaml", package = pkgname)
  default.rate.template <<- system.file("rate.template.yaml", package = pkgname)
  default.surge.template <<- system.file("surge.template.yaml", package = pkgname)
  default.error.template <<- system.file("error.template.yaml", package = pkgname)
  last.alert <<- origin
}
