test_read_json_part <- function(dest = NA){

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "BStGer","Beschwerdekammer_RH")
  }

  url <- "https://bstger.weblaw.ch/?size=n_500_n&filters%5B0%5D%5Bfield%5D=court&filters%5B0%5D%5Bvalues%5D%5B0%5D=Beschwerdekammer%3A%20Rechtshilfe&filters%5B0%5D%5Btype%5D=any#/"
  #... use developper tools, search for
  urlJSON <- "https://bstger.weblaw.ch/api/.netlify/functions/searchQueryService"
  #  Memo:
  #  unfortunatiely this link above cannot be used directly in another tab
  #  service is running indefinitely
  #  therefore I use the developper toolbox and look for the result of
  #  this request

  # ... manually store json
  fi <- "./data/20220602_bstger_parts.json"
  fi <- "./data/20220711_bstger.json"

  txt <- readLines(fi)

  m <- gregexpr("https\\:\\/\\/bstger.weblaw.ch\\/files\\/[0-9]{8}_[A-Z]{2}_[0-9]{4}.{1,20}\\.pdf", txt, perl = TRUE)
  fls <- regmatches(txt, m)[[1]]


  # Download PDF Files
  for(i in 1:length(fls)){
    f <- fls[i]
    y <- substr(f, 32, 35)
    if(as.integer(y) %in% 2000:2022){
      d <- file.path(dest, y, basename(f))
      if(!file.exists(d)){
        download.file(f, d, mode="wb")
      }

    }
  }

}
