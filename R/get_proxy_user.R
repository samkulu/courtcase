#' Get Proxy User from stored file
#'
#' Proxy credentials can be stored in a file. Make sure that
#' this information is secured. Hide it and place it in a
#' encrypted folder.
#'
#'
#' @return
#' @export
#'
#' @examples
#' get_proxy_user()
get_proxy_user <- function(path = Sys.getenv("R_USER"),
                           file = ".proxyusr"){
  # Stored credentials in a hidden file in a encrypted folder
  filename <- file.path(path, file)
  # Return
  if(file.exists(filename))
    return(readLines(filename, warn = FALSE))
  else
    return(NA)
}
