#' Download BGer Decisions by Doc-ID
#'
#' @param docID
#' @param dest
#'
#' @return Files
#' @export
#'
#' @examples
#' download_bger_docid("144-I-1","test.html")
#' download_bger_docid("144-I-20","test.html") # Provoke a warning!
download_bger_docid <- function(docID, dest, tryN = 5, isPublic = TRUE){
  cat("Download ", docID)
  if (file.exists(dest)) {
    cat(" exists \n")
    return(NULL)
  }  else {
    cat(" ... \n")
  }

  # Download
  if (isPublic){
    # BGE Leitentscheide
    url <- "https://www.bger.ch/ext/eurospider/live/de/php/clir/http/index.php?lang=de&type=show_document&highlight_docid=atf://%docID%:de&print=yes"
  } else {
    # Weitere Urteile ab 2000
    url <- "https://www.bger.ch/ext/eurospider/live/de/php/aza/http/index.php?lang=de&type=show_document&highlight_docid=aza://%docID%&print=yes"
  }

  url <- gsub("%docID%",docID,url)

  saveFile <- function(url){
    # Download
    page <- read_link(url)
    # Prevent print menue onLoad
    txt <- "window.print()"
    page2 <- gsub(txt,"",page)
    # Workaround for dest=NA
    if(is.na(dest)){
      # return txt
      return(page2)
    } else {
      # Save at destination
      con <- file(dest)
      writeLines(page2,con)
      close(con)
    }
  }

  if(TRUE){
    saveFile(url)
  } else {
    # Not working properly !! Since 2023
    # TRY several times
    count <- -tryN
    while(count < 0){
      result = tryCatch({
        saveFile(url)
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

}
