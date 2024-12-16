#' Get Proxy-Server from Browser
#'
#' @return
#' @export
#'
#' @examples
#' get_proxy()
get_proxy <- function(verbose = FALSE){
  require(curl)
  if (verbose){
    cat("Curl version: ", curl::curl_version()$version, "\n")
    cat("Has internet: ", curl::has_internet(),  "\n")
    cat("Proxy info: \n")
    print(str(curl::ie_proxy_info()))
  }

  # Return Proxy
  curl::ie_get_proxy_for_url('https://www.google.com')
}
