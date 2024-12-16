#' Download BStGer Weekly
#'
#' It's important to retrieve the data not in bulk but in smaller bits.
#' The website is updating the request asynchronously. Having a too large
#' request means, that we had to scroll down in the browser. This is not
#' feasible within the httr framework and RSelenium is not a good joice
#' because a Java runtime is needed.
#'
#' @param startDate
#' @param endDate
#'
#' @return
#' @export
#'
#' @examples
#' archive <- download_bstger(startDate = as.Date("2021-05-17"), endDate = as.Date("2021-05-17"))
#' archive <- download_bstger()
#'
#' write_xl(archive)
download_bstger <- function(startDate = as.Date("2024-01-01"),
                            endDate = Sys.Date(),
                            host = "https://bstger.weblaw.ch"){

    # Find Destination
    set_user()

    # Check Dates
    dts <- seq(startDate, endDate, "weeks")

    # Referer-Url Pattern
    # "https://bstger.weblaw.ch/dashboard?guiLanguage=de&filters=[[\"publicationDate\",[{\"from\":\"2024-11-01T00:00:00.000Z\",\"to\":\"2024-11-08T22:59:59.999Z\"}]],[\"court\",[\"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance\"]]]"
    #  https://bstger.weblaw.ch/dashboard?guiLanguage=de&filters=[[%22publicationDate%22,[%7B%22from%22:%222024-11-01T00:00:00.000Z%22,%22to%22:%22toDateT22:59:59.999Z%22%7D]],[%22court%22,[%22Beschwerdekammer:%20Rechtshilfe;;Cour%20des%20plaintes:%20entraide%20p%C3%A9nale;;Corte%20dei%20reclami%20penali:%20assistenza%20giudiziaria;;Board%20of%20Appeal:%20Legal%20Assistance%22]]]
    tmp <- "https://bstger.weblaw.ch/dashboard?guiLanguage=de&filters=[[\"publicationDate\",[{\"from\":\"%fromDate%T00:00:00.000Z\",\"to\":\"%toDate%T22:59:59.999Z\"}]],[\"court\",[\"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance\"]]]&sort-field=relevance&sort-direction=relevance"


    # Storage for document info
    json_archive <- list()

    # Iteration
    for(i in 1:length(dts)){
      #dt <- as.Date("2024-01-01")
      dt <- dts[i]
      fromDate <- dt
      toDate <- dt + 7
      u <- tmp
      u <- gsub("%fromDate%", fromDate, u)
      u <- gsub("%toDate%", toDate, u)
      # Url Encode
      u <- URLencode(u)
      u <- gsub("\\[", "%5B", u)
      u <- gsub("\\]", "%5D", u)
      u <- gsub(",", "%2C", u)
      u <- gsub(";", "%3B", u)
      u <- gsub(":", "%3A", u)
      u <- gsub("https%3A", "https:", u)
      message("Week from ", dt);cat(u, "\n")


      # Check Proxy
      if(!exists("proxy_user")) proxy_user <<- get_proxy_user()
      if(is.na(proxy_user[1])) stop("Proxy user for authentication missing!")

      # Proxy-Server
      httr::set_config(
        httr::use_proxy(url = get_proxy(), username=proxy_user[1],password = proxy_user[2] , auth="any"),
        override = TRUE
      )

     # Header w/o cookie is working fine on 2024-12-12 (MES)
      h <-    add_headers(
        `Accept` = "*/*",
        `Accept-Encoding` = "gzip, deflate, br",
        `Accept-Language` = "de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6",
        `Connection` = "keep-alive",
        `Content-Type` = "text/plain;charset=UTF-8",
        #`Cookie` = cookie,
        `Host` = "bstger.weblaw.ch",
        `Origin` = "https://bstger.weblaw.ch",
        `Referer` = "https://bstger.weblaw.ch/dashboard?guiLanguage=de&filters=%5B%5B%22publicationDate%22%2C%5B%7B%22from%22%3A%222024-11-04T00%3A00%3A00.000Z%22%2C%22to%22%3A%222024-11-11T22%3A59%3A59.999Z%22%7D%5D%5D%2C%5B%22court%22%2C%5B%22Beschwerdekammer%3A%20Rechtshilfe%3B%3BCour%20des%20plaintes%3A%20entraide%20p%C3%A9nale%3B%3BCorte%20dei%20reclami%20penali%3A%20assistenza%20giudiziaria%3B%3BBoard%20of%20Appeal%3A%20Legal%20Assistance%22%5D%5D%5D&sort-field=relevance&sort-direction=relevance",
        `sec-ch-ua` = '"Microsoft Edge";v="131", "Chromium";v="131", "Not_A Brand";v="24"',
        `sec-ch-ua-mobile` = "?0",
        `sec-ch-ua-platform` = "Windows",
        `sec-fetch-dest` = "empty",
        `sec-fetch-mode` = "cors",
        `sec-fetch-site` = "same-origin",
        `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/131.0.0.0 Safari/537.36 Edg/131.0.0.0"
      )

      # # Working Examples
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-01T00:00:00.000Z","to":"2024-11-08T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_zza6bz6","sessionDuration":5977,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_209m6m7","sessionDuration":19,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_yr87sw5","sessionDuration":717,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"100"}})"
      #

      # Request with Data in Post
      body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"%fromDate%T00:00:00.000Z","to":"%toDate%T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"100"}})"
      body <- gsub("%fromDate%", fromDate, body)
      body <- gsub("%toDate%", toDate, body)
      json_data <- jsonlite::parse_json(body)

      url <- "https://bstger.weblaw.ch/api/.netlify/functions/searchQueryService"
      responseJSON <- httr::POST(url, h, body = json_data, encode = "json")


      # Check Response
      if(httr::status_code(responseJSON) == "200"){
        json <- httr::content(responseJSON, as="text", encoding = "UTF8")
        df <- jsonlite::parse_json(json)
        N <- df$totalNumberOfDocuments

        for (d in df$documents){
          # Finding BGE Entscheide
          # if(is.null(decision$ENTSCHEIDSTATUS)) browser()
          # BGESTATUS = unlist(d$metadataKeywordTextMap$bgeStatus)

          # Add to Archive
          decision <- tibble_row(
                          LEID = unlist(d$metadataKeywordTextMap$leid),
                          LANGUAGE = unlist(d$metadataKeywordTextMap$language),
                          REFERENZ = unlist(d$metadataKeywordTextMap$title),  # d$metadataKeywordTextMap$sortTitle[[1]]
                          CONTENT = d$content,
                          URL =  unlist(d$metadataKeywordTextMap$originalUrl),
                          KAMMER = unlist(d$metadataKeywordTextMap$court),
                          ENTSCHEIDDATUM = unlist(d$metadataDateMap$rulingDate),  # d$metadataKeywordTextMap$sortRulingDate[[1]]
                          PUBLUKATIONSDATUM = unlist(d$metadataDateMap$publicationDate),    #  d$metadataKeywordTextMap$sortPublicationDate[[1]]
                          ENTSCHEIDTYP = unlist(d$metadataKeywordTextMap$rulingType),   # BStGer

                          # ENTSCHEIDSTATUS
                          # 1st BStGer  ... unlist(d$metadataKeywordTextMap$tipoSentenza), # Kein Weiterzug
                          # 2nd BGE if BStGer is NULL
                          ENTSCHEIDSTATUS = paste0(unlist(d$metadataKeywordTextMap$tipoSentenza),
                                                   unlist(d$nestedMetadataMap$bgeDossier)[[1]])
                        )

          # Cache Storage
          json_archive[[d$leid]] <- decision

          # Download-Link
          pdf_link <- paste0(host, decision$URL)
          message(pdf_link)

          # Download to Destination
          fi <- basename(pdf_link)
          yr <- substr(fi, 1,4)
          dest <- gsub("DEST=", "", user$DEST)
          fullname <- file.path(dest, "BStGer/Beschwerdekammer_RH", yr, fi)
          if(!file.exists(fullname))
            download.file(pdf_link, fullname, mode="wb")
        }

      }

    }

    # Data Collection
    result <- json_archive %>%
              do.call(dplyr::bind_rows, .) %>%
              mutate(
                URL = paste0(host, URL),
                ENTSCHEIDTYP = gsub("^(.*?);;.*", "\\1", ENTSCHEIDTYP),
                CONTENT = gsub("^(.*?);;.*", "\\1", CONTENT),
                KAMMER = gsub("^(.*?);;.*", "\\1", KAMMER),
                ENTSCHEIDSTATUS = gsub("^(.*?);;.*", "\\1", ENTSCHEIDSTATUS),
                ENTSCHEIDDATUM = as.POSIXct(ENTSCHEIDDATUM),
                PUBLUKATIONSDATUM = as.POSIXct(PUBLUKATIONSDATUM)
              )

    attr(result, "Name") <- "Gerichtsentscheide BStGer"

    # Return
    return(result)
}
