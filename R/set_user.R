#' Set Gotham User Credentials
#'
#' @param usr
#' @param pwd
#'
#' @return
#' @export
#'
#' @examples
#' set_user()
set_user <- function(usr = NA, pwd = NA, dest = NA){
  if (is.na(usr) & is.na(pwd)){
    user <- readLines(file.path(Sys.getenv("R_USER"),".courtcase"),
                      warn = FALSE)
    names(user) <- c("DEST")

    user <<- as.list(user)
  }  else {
    user <<- list(USER = usr, PASSWORD = pwd, DESTINATION = dest)
  }
}
