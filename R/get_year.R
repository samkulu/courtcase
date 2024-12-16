#' Get (current) Year
#'
#' @return Scalar
#' @export
#'
#' @examples
get_year <- function(dts = NA){
  if (is.na(dts)) dts <- Sys.time()
  as.integer(format(dts,"%Y"))
}
