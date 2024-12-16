#' Download BGer Further
#'
#' @param year
#' @param month
#' @param removeLastPageCache
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' download_bger_further()
#' download_bger_further(2022)
download_bger_further <- function(year = get_year(),
                                  month=1:12, removeLastPageCache = TRUE,
                                  dest = NA){

  #Alternative?  url <- "http://www.servat.unibe.ch/dfr/dfr_bger2001.html"

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "BGer_WeitereUrteile")
  }
  DEST <- dest
browser()

  # Start Date
  sDate = as.Date(paste(year,1,1,sep = "-"))
  sMonth <- seq(sDate, by = "month", length = 12)

  # Remove unwanted months
  idx <- substr(sMonth,5,8) %in% paste("-",sprintf("%02d",month),"-",sep="")
  sMonth <- sMonth[idx]

  # Filter for past only
  sMonth <- sMonth[sMonth < Sys.Date()]

  # Clear Cache for last page for complete download next time
  if (removeLastPageCache) {
    if (year == get_year()) {
      on.exit(clear_cache_bger(year))
    }
  }

  # Functions ####
  getLastPage <- function(url, tryN = 3){
    require(XML)
    # BGer has max 2000 entries, meaning 200 pages
    # Read from Internet
    urlMax <- gsub("%Page%",200,url)

    if(TRUE){
      # Code that is needed
      page <- read_link(urlMax)
    } else {
      # TRY several times
      count <- -tryN
      while(count < 0){
        result = tryCatch({
          # Code that is needed
          page <- read_link(urlMax)

          count <- 1
        }, warning = function(warning_condition) {
          # warning-handler-code
          message("Warning handling: ", url)
          count <- count + 1
          cat("retry download another ", count, " times \n")
        }, error = function(error_condition) {
          #error-handler-code
          message("Error handling: ", url)
          count <- 0
          # stop
        }, finally={
          # cleanup-code
          # message("File probably not unavailable!")
        })
      }
    }




    # Parsing stuff
    # doc <- htmlTreeParse(page, useInternalNodes = TRUE)

    # Shortcut
    idx <- suppressWarnings(grep("R&auml;nge",page))
    if (length(idx) == 0) message("Download finished") # Result will be -Inf

    tmp <- paste("<a href='url' ",page[idx])
    doc <- htmlParse(tmp)
    vals <- unlist(xpathApply(doc, '//a', xmlValue))
    vals <- suppressWarnings(as.numeric(vals))
    max(vals, na.rm = TRUE)
  }

  getLinkCache <- function(url, dest = DEST){
    # Parameters
    sPo <- as.integer(gregexpr("page",url))
    ePo <- as.integer(gregexpr("from_",url))
    page <- substr(url,sPo + 5, ePo - 2)
    sPo <- as.integer(gregexpr("from_",url))
    dtFrom <- substr(url,sPo + 10, sPo + 19)
    sPo <- as.integer(gregexpr("to_",url))
    dtTo <- substr(url,sPo + 8, sPo + 17)

    # Key file name
    key <- paste(dtFrom,"-",dtTo,"-(",page,").txt", sep = "")

    # Folder Destination
    path <- file.path(dest,".cache")
    if (!dir.exists(path)) dir.create(path, recursive = TRUE)
    # Fullname
    fi <- file.path(path, key)
    if (file.exists(fi)) {
      message(key)
      return(NULL)
    } else {
      writeLines(url, fi)
      return(fi)
    }
  }

  getDocID <- function(url, doCache = TRUE){
    # Cache downloaded urls for single processing
    if (doCache) {

      cc <- getLinkCache(url)
      if (is.null(cc)) return(NULL)
    }

    if(TRUE){
      page <- read_link(url)
    } else {
      # Try several times
      count <- -3
      while(count < 0){
        tryCatch({
          page <- read_link(url)
          count <- 1
        }, warning = function(warning_condition) {
          # warning-handler-code
          count <- count + 1
          cat("retry getCodeID another ", count ," times \n")
        }, error = function(error_condition) {
          # error-handler-code
          count <- 0
        }, finally={
          # cleanup-code
        })
      }
    }






    idx <- grep("Seite mit hervorgehobenen Suchbegriffen",page)
    url <- paste("<a href='url' ", page[idx])
    doc <- htmlParse(url)
    docID <- unlist(xpathApply(doc, '//a', xmlValue))
    webID <- docID

    # Unfortunately there are some "%y" like "/99" "/00"
    # Workaround for /99 /98 /00 / 01 etc.
    # Unsufficient:
    # webID <- gsub("\\/99","\\/1999",webID)
    # webID <- gsub("\\/98","\\/1998",webID)
    rr <- substr(webID,as.integer(regexpr("\\/",webID)),nchar(webID))
    idx <- which(nchar(rr) == 3)  # < 5
    if (length(idx)) {
      x <- as.integer(substr(rr,2,3))
      y <- ifelse(x >= 90 & x < 100, 1900 + x, ifelse(x >= 0 & x < 90, 2000 + x, x))
      z <- paste("/", y, sep = "")
      for (i in idx)  webID[i] <- gsub(rr[i],z[i],webID[i])
    }

    webID <- gsub("\\.","-",webID)
    webID <- gsub("\\/","-",webID)

    # Unfortunately some " " are replaced by "_"
    # instead of "-". See
    # 01-02-2000-I_421-1998
    # 01-02-2000-I-421-1998
    # 01-02-2000-I-573-1999
    # 01-02-2000-I_573-1999
    webID <- paste(substr(webID,1,10),
                   substr(webID,12,nchar(webID)),
                   sep = "-"
    )
    webID <- gsub(" ","_",webID)
    if (regexpr("201998|201999",webID)[1] > 0) browser()

    webID
  }

  getClass <- function(page, name = "para"){
    doc <- htmlTreeParse(page, useInternalNodes = TRUE)
    class_xp <- "//div[@class='%name%']//text()"
    class_xp <- gsub("%name%", name, class_xp)
    xpathSApply(doc,class_xp,xmlValue)
  }

  getIDorClass <- function(page, idName="txt2", className = "para"){
    # See https://stackoverflow.com/questions/27798888/parsing-html-elements-by-id-and-class-with-xml-package
    doc <- htmlTreeParse(page, useInternalNodes = TRUE)
    id_or_class_xp <- "//p[@id='%idName%']//text() | //div[@class='%className%']//text()"
    id_or_class_xp <- gsub("%idName%", idName, id_or_class_xp)
    id_or_class_xp <- gsub("%className%", className, id_or_class_xp)
    xpathSApply(doc,id_or_class_xp,xmlValue)
  }

  getTitle <- function(page){
    doc <- htmlTreeParse(page, useInternalNodes = TRUE)
    xpathSApply( doc,"//i",xmlValue)
  }

  getSubDirByTitle <- function(txt){
    txt <- gsub("Â", "",txt)
    txt <- gsub(" ", "", txt)
    ss <- strsplit(txt, split = "\\.")[[1]]
    ss[1]
  }

  getDest <- function(docID = NA, create = FALSE, dest = DEST){
    if (is.na(docID)) return(dest)
    # chamber <- substr(docID,12,13)
    # year <- substr(docID,nchar(docID)-3,nchar(docID))
    # dt <- as.Date(substr(docID,1,10),format="%d-%m-%Y")
    year <- substr(docID,7,10)
    month <- substr(docID,4,5)
    day <- substr(docID,1,2)
    path <- file.path(DEST,year,month,day)
    # Create directory if not exists
    if (create && !dir.exists(path)) dir.create(path, recursive = TRUE)
    # Return
    file.path(path, paste(docID,".html",sep = ""))
  }

  delEmpty <- function(dest = DEST){
    fls <- list.files(DEST, recursive = TRUE, full.names = TRUE)
    minSize <- min(file.size(fls))
    idx <- which(file.size(fls) <= 3100) # minSize: 2620 2629 3081 3082
    print(fls[idx])
    browser()
    if (length(idx) > 0) file.remove(fls[idx])
  }

  getPagesMonthly <- function(sMonth){
    #dtFrom <- "1.1.2000"
    #dtTo   <- "1.2.2000"
    #url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&page=%Page%&from_date=%dtFrom%&to_date=%dtTo%&sort=relevance&insertion_date=&top_subcollection_aza=all&query_words="
    #url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&query_words=&lang=de&top_subcollection_aza=all&from_date=01.01.2000&to_date=01.02.2000&x=0&y=0"
    #url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&query_words=&lang=de&top_subcollection_aza=all&from_date=01.01.2000&to_date=01.02.2000&x=0&y=0"
    #url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&page=%Page%&from_date=%dtFrom%&to_date=%dtTo%&sort=relevance&insertion_date=&top_subcollection_aza=all&query_words="

    #url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&page=1&from_date=01.01.2022&to_date=01.02.2022&sort=relevance&insertion_date=&top_subcollection_aza=all&query_words="
    url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=simple_query&page=%Page%&from_date=%dtFrom%&to_date=%dtTo%&sort=relevance&insertion_date=&top_subcollection_aza=all&query_words="


    for (i in 5:length(sMonth)) {
      dtFrom <- format(sMonth[i],"%d.%m.%Y")
      eDate <- seq(sMonth[i], by = "month", length = 2)[2]
      if (eDate > Sys.Date()) eDate <- Sys.Date()-1
      dtTo   <- format(eDate,"%d.%m.%Y")
      tmp <- url
      tmp <- gsub("%dtFrom%", dtFrom, tmp)
      tmp <- gsub("%dtTo%", dtTo, tmp)
      #Download Pages
      getPages(tmp)
    }

    # Close all open connections
    closeAllConnections()
  }

  getPages <- function(url){
    # Show all available pages
    n <- getLastPage(url)
    if (is.infinite(n)) return(NULL)

    pages <- 1:n
    # Download Page by Page
    for (p in pages) {
      tmp <- gsub("%Page%", p,url)
      docIDs <- getDocID(tmp)

      # File by File
      for (docID in docIDs) {
        dest <- getDest(docID, create = TRUE, dest)
        # Download
        download_bger_docid(docID, dest, isPublic = FALSE)
      }

    }
  }


  # Test Successfull
  # download_bger_docid("03-01-2000-4C-358-1999", "test.html", isPublic = FALSE)

  # Run Download
  getPagesMonthly(sMonth)
}
