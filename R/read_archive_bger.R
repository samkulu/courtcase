#' Archive of BGE and EMGR
#'
#' See https://www.bger.ch/ext/eurospider/live/de/php/clir/http/index_atf.php
#'
#' @param year
#' @param type
#'
#' @return Dataframe
#' @export
#'
#' @examples
#' read_archive_bger()
#' read_archive_bger(2022,"I")
read_archive_bger <- function(year = format(Sys.Date(),"%Y"), type = "I"){
  if(is.character(year)) year <- as.integer(year)
  yearZero <- 1874
  yearBGer <-  year - yearZero
  url <- "https://www.bger.ch/ext/eurospider/live/de/php/clir/http/index_atf.php?year=%yearBGer%&volume=%type%"
  url <- gsub("%yearBGer%", yearBGer, url)
  url <- gsub("%type%", type, url)

  # Workaround for
  # page <- readLines(url, encoding = "utf8", warn = FALSE)
  page <- read_link(url)

  doc = XML::htmlParse(page, encoding = "utf8")
  # tableNodes = getNodeSet(doc, "//li")
  links <- xpathSApply(doc, "//a/@href")
  free(doc)

  # Get Doc IDS
  getIDs <- function(x){
    x <- unname(links)
    idx <- grep("docid",x)
    x <- x[idx]
    pat1 <- "docid=atf%3A%2F%2"
    pat2 <- "%3A&"
    sPo <- unlist(gregexpr(pat1,x))
    ePo <- unlist(gregexpr(pat2,x))

    x <- substring(x, sPo + nchar(pat1) + 1, ePo)
    x <- gsub("%","",x)
    x
  }

  docIDs <- getIDs(links)
  docIDs

}
