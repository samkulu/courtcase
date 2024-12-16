#' Test Proxy-Server
#'
#' @return
#' @export
#'
#' @examples
#' proxy_user <- c("username","password")
#' test_proxy(proxy_user)
#' test_proxy()
test_proxy <- function(proxy_user = NA){
  require(httr)

  if (is.na(proxy_user))
    proxy_user <- get_proxy_user()


  # GET
  tmp <- httr::GET("http://had.co.nz",
                   httr::use_proxy(get_proxy(),
                                   username = proxy_user[1],
                                   password = proxy_user[2],
                                   auth = "any"),
                   verbose = TRUE)
  # HTTP-Statuscode 200 is a success
  if(httr::status_code(tmp) == 200)
    return(TRUE)
  else
    return(FALSE)
}
