download_bstger <- function(){
  # https://bstger.weblaw.ch/?size=n_60_n#/
  # https://bstger.weblaw.ch/?size=n_100_n#/
  # https://links.weblaw.ch/

  url <- "https://bstger.weblaw.ch/?size=n_100_n&filters%5B0%5D%5Bfield%5D=court&filters%5B0%5D%5Bvalues%5D%5B0%5D=Beschwerdekammer%3A%20Rechtshilfe&filters%5B0%5D%5Btype%5D=any"
  url <- "https://bstger.weblaw.ch/?size=n_500_n&filters%5B0%5D%5Bfield%5D=court&filters%5B0%5D%5Bvalues%5D%5B0%5D=Beschwerdekammer%3A%20Rechtshilfe&filters%5B0%5D%5Btype%5D=any"
  page <- read_link(url)



  JS1 <- read_link("https://bstger.weblaw.ch/static/js/2.c3095fe7.chunk.js")
  JS2 <- read_link("https://bstger.weblaw.ch/static/js/main.30237468.chunk.js")



  # Check Proxy
  if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
  if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")

  # Proxy-Server
  proxy <- get_proxy()

  httr::set_config(
    use_proxy(url = proxy, username=proxy_user[1],password = proxy_user[2] , auth="any"),
    override = TRUE
  )

  response <- httr::GET(url)



  if(response$status_code == 200){
    # As Usual
    httr::content(response, as="text")

  } else if(response$status_code >= 400){
    # Error
  }


  url <- "https://bstger.weblaw.ch/?size=n_50_n&filters%5B0%5D%5Bfield%5D=court&filters%5B0%5D%5Bvalues%5D%5B0%5D=Beschwerdekammer%3A%20Rechtshilfe&filters%5B0%5D%5Btype%5D=any"
  urlJSON <- "https://bstger.weblaw.ch/api/.netlify/functions/searchQueryService"
  urlJSON <- "https://bstger.weblaw.ch/api/.netlify/functions/searchQueryService"

  postData <- r"({"sortOrder":"desc","sortField":"publicationDate","size":500,"guiLanguage":"de","metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe"},"userID":"_r5jhzy7m6","sessionDuration":1657534832,"origin":"Dashboard","aggs":{"fields":["rulingType","tipoSentenza","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":10}})"

  # GET Json Response
  h <- httr::add_headers(
    `Accept` = "*/*",
    `Accept-Encoding` = "gzip, deflate, br",
    `Accept-Language` = "de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
    # `Cache-Control` = "max-age=0",
    `Connection` = "keep-alive",
    `Content-Length` = "561",
    `Content-Type` = "text/plain;charset=UTF-8",
    #`Cookie` = "FTNT-EP-FG3K6ETB20900010=p6Wlpc6zBbakpaWluKWlpfCdlZ2Wk5CSlOXg7_Xhi-zr8ffki-Th6Ozri-btpaWl",
    #`Cookie` = "FTNT-EP-FG3K6ETB20900010=p6WlpchZ7O2kpaWluKWlpfCdlZ2Wk5CSlOXg7_Xhi-zr8ffki-Th6Ozri-btpaWl",
    `Cookie` = "FTNT-EP-FG3K6ETB20900010=p6WlpdhtIOykpaWluKWlpfCdlZ2Wk5CSlOXg7_Xhi-zr8ffki-Th6Ozri-btpaWl",
    `Host` = "bstger.weblaw.ch",
    `Origin` = "https://bstger.weblaw.ch",
    `Referer` =  url, #
    # `sec-ch-ua` =  "\" Not;A Brand\";v=\"99\", \"Microsoft Edge\";v=\"96\", \"Chromium\";v=\"96\"",
    `sec-ch-ua` = "\" Not A;Brand\";v=\"99\", \"Chromium\";v=\"101\", \"Microsoft Edge\";v=\"101\"",
    `sec-ch-ua-mobile` = "?1",
    `sec-ch-ua-platform` = "Android",
    `Sec-Fetch-Dest` = "empty",
    `Sec-Fetch-Mode` = "no-cors",
    `Sec-Fetch-Site` = "same-origin",
    #`Sec-Fetch-User` = "?1",
    #`Upgrade-Insecure-Requests` = "1",
    `User-Agent` = "Mozilla/5.0 (Linux; Android 6.0; Nexus 5 Build/MRA58N) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/101.0.4951.54 Mobile Safari/537.36 Edg/101.0.1210.39"
  )


  responseJSON <- httr::POST(urlJSON, h)
  responseJSON <- httr::POST(urlJSON, h, accept_json())

  object.size(responseJSON)

  if(responseJSON$status_code == 200){
    httr::content(responseJSON, as="text")
  }


}
