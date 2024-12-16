#' Download latest BStGer files
#'
#' @param url
#' @param dest
#'
#' @return
#' @export
#'
#' @examples
#' download_bstger_list()
download_bstger_list <- function(url = "http://www.bstger.ch",
                                 dest = NA,
                                 pattern = "_RR_|_RH_|_RP_"){
  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "BStGer/Beschwerdekammer_RH")
  }

  library(rvest)
  html_page <- read_html(url)
  df <- html_page %>%
        rvest::html_nodes(".col-md-12") %>%
        # rvest::html_nodes(".list-group-header") %>%
        # rvest::html_nodes(".scroll-y") %>%
        rvest::html_elements(".list-group-item")


  # txt <- df %>% html_text() # Reference
  # dts <- gsub("\n", "", txt) %>%
  #        gsub(".* ([0-9]{2}\\.[0-9]{2}\\.[0-9]{4}).*", "\\1", .)

  # Links
  lnks <- df %>% html_nodes("a") %>% html_attr("href")

  # Download RR-Files
  rr_decisions <- lnks[grepl(pattern, lnks)]
  filename <- basename(rr_decisions)
  fullname <- file.path(dest, substr(filename, 1, 4), filename)

  # Download if not exists already
  for(i in 1:length(rr_decisions)){
    if (!file.exists(fullname[i]))
      try(download.file(rr_decisions[i], fullname[i], mode="wb"))
  }


}
