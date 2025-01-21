run_decisions <- function(){

  # Year
  year <- get_year() - 1

  # BGer Leitentscheide
  download_bger(year,"I")
  download_bger(year,"II")
  download_bger(year,"IV")

  # BGer Weitere Urteile
  download_bger_further(year = year)

  # BStGer Entscheide
  sDate <- as.Date(paste0(year, "-01-01"))
  archive <- download_bstger(startDate = sDate)
}
