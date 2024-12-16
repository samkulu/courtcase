#' Test IP
#'
#' @param dns
#'
#' @return
#' @export
#'
#' @examples
#' test_ip()
test_ip <- function(dns = "google.ch"){
  cmd <- paste0("ping -cS ", dns)
  system(cmd)
}
