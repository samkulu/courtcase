read_link <- function(url){
  tmp <- tempfile()
  download.file(url, tmp)
  page <- readLines(tmp, encoding = "utf8", warn = FALSE)
  return(page)
}
