#' Download Files from BGer
#'
#' @param year
#' @param type
#' @param dest
#'
#' @return Files
#' @export
#'
#' @examples
#' download_bger(2022,"I")
#' download_bger(2022,"II")
#' download_bger(2022,"IV")
#' # Download whole history
#' yrs <- 1954:2023
#' for(year in yrs) download_bger(year,"I")
#' for(year in yrs) download_bger(year,"II")
#' for(year in yrs) download_bger(year,"IV")
download_bger <- function(year, typ, dest = NA){

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "BGer_Leitentscheide", "%sub%/%year%")
  }

  # Archive
  # See e.g. https://www.bger.ch/ext/eurospider/live/de/php/clir/http/index_atf.php?year=144&volume=I
  docIDs <- read_archive_bger(year,typ)
  if (length(docIDs) == 0) return(NULL)

  # Subfolder
  subDir <- switch(typ,
                   "I" = "I Verfassungsrecht",
                   "II" = "II Verwaltungsrecht und Internationales oeffentliches Recht",
                   "III" = "III Zivilrecht Schuldbetreibungs- und Konkursrecht",
                   "IV" = "IV Strafrecht und Strafvollzug",
                   "V" = "V Sozialversicherungsrecht"
  )
  dest2 <- gsub("%sub%",subDir,dest)
  dest2 <- gsub("%year%",year,dest2)
  if (!dir.exists(dest2)) dir.create(dest2, recursive = TRUE)

  # Name Output files
  filename <- paste(docIDs,".html",sep="")
  dest2 <- file.path(dest2, filename)

  # Process download
  for(i in 1:length(filename)){
    download_bger_docid(docIDs[i], dest2[i], isPublic = TRUE)
  }

}
