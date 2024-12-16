#' GET IP
#'
#' @param dns
#'
#' @return
#' @export
#'
#' @examples
#' get_ip()
get_ip <- function(dns = "gothamcity.ch"){
  curl::nslookup(dns)
}
