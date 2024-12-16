#' Write Excel-File
#'
#' You can use a named list to store the different tables into Excel-Sheets.
#' The Excel comes as raw file without heavy formatting.
#'
#' @param data
#' @param tmp
#'
#' @return
#' @export
#'
#' @examples
write_xl <- function(data, tmp = FALSE){
  if(tmp){
    # Create Dummy Excel
    return(writexl::write_xlsx(data))
  } else {
    # Use Name in Attribute
    filename <- paste0(attr(data, "Name"), ".xlsx")
    writexl::write_xlsx(data, filename)
  }
}
