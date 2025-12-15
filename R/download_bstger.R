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
download_bstger <- function(startDate = as.Date("2025-09-01"),
                            endDate = Sys.Date(),
                            host = "https://bstger.weblaw.ch"){

    # Find Destination
    set_user()

    # Check Dates
    dts <- seq(startDate, endDate, "weeks")

    # Referer-Url Pattern
    tmp <- "https://bstger.weblaw.ch/?filters=[[\"court\",[\"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance\"]],[\"publicationDate\",[{\"from\":\"2025-09-01\",\"to\":\"2025-09-07\"}]]]&sort-field=relevance&sort-direction=relevance" # New 2025-12-15

    # Storage for document info
    json_archive <- list()

    # Iteration
    for(i in 1:length(dts)){
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

      # Make a Session with pseudo authentication
      # Using Authentication without login
      make_session <- httr::GET(url = "https://bstger.weblaw.ch/api/getKeycloakConfigurations")
      cookie <- cookies(make_session)$value[1]


      # Header w/o cookie is working fine on 2024-12-12 (MES)
      h <-    httr::add_headers(
        `Accept` = "*/*",
        `Accept-Encoding` = "gzip, deflate, br",
        `Accept-Language` = "de,de-DE;q=0.9,en;q=0.8,en-GB;q=0.7,en-US;q=0.6,de-CH;q=0.5,rm;q=0.4",
        `Access-Control-Allow-Origin` = "*",
        `Connection` = "keep-alive",
        #`Content-Type` = "text/plain;charset=UTF-8",
        `Content-Type` = "application/json",
        # `Cookie` = cookie,
        `Host` = "bstger.weblaw.ch",
        `Origin` = "https://bstger.weblaw.ch",
        `Referer` = "https://bstger.weblaw.ch/?filters=%5B%5B%22court%22%2C%5B%22Beschwerdekammer%3A%20Rechtshilfe%3B%3BCour%20des%20plaintes%3A%20entraide%20p%C3%A9nale%3B%3BCorte%20dei%20reclami%20penali%3A%20assistenza%20giudiziaria%3B%3BBoard%20of%20Appeal%3A%20Legal%20Assistance%22%5D%5D%2C%5B%22publicationDate%22%2C%5B%7B%22from%22%3A%222025-09-01%22%2C%22to%22%3A%222025-09-08%22%7D%5D%5D%5D&sort-field=relevance&sort-direction=relevance", # New 2025-12-15 / Yes there is still a Date in the string !!
        `sec-ch-ua` = '"Chromium";v="142", "Microsoft Edge";v="142", "Not_A Brand";v="99"', # new 2025-12-15
        `sec-ch-ua-mobile` = "?0",
        `sec-ch-ua-platform` = "Windows",
        `sec-fetch-dest` = "empty",
        `sec-fetch-mode` = "cors",
        `sec-fetch-site` = "same-origin",
        `user-agent` = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/142.0.0.0 Safari/537.36 Edg/142.0.0.0"
      )

      # Older Working Examples
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-01T00:00:00.000Z","to":"2024-11-08T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_zza6bz6","sessionDuration":5977,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_209m6m7","sessionDuration":19,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"userID":"_yr87sw5","sessionDuration":717,"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"10"}})"
      # body <- r"({"guiLanguage":"de","metadataDateMap":{"publicationDate":{"from":"2024-11-04T00:00:00.000Z","to":"2024-11-11T22:59:59.999Z"}},"metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"aggs":{"fields":["rulingType","tipoSentenza","bgeStatus","year","court","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"100"}})"
      #

      # Request with Data in Post
      # Size 100 hacked into, maybe not necessary
      body <- r"({"guiLanguage":"de","metadataKeywordsMap":{"court":"Beschwerdekammer: Rechtshilfe;;Cour des plaintes: entraide pénale;;Corte dei reclami penali: assistenza giudiziaria;;Board of Appeal: Legal Assistance"},"metadataDateMap":{"publicationDate":{"from":"%fromDate%","to":"%toDate%"}},"userID":"_fltt5hx","from":0,"aggs":{"fields":["filterDate","sortPublicationDate","sortRulingDate","publicationDate","rulingDate","rulingType","tipoSentenza","bgeStatus","bgeDossierList","bstgerDossierList","year","court","tpfDossierList","author","language","lex-ch-bund-srList","ch-jurivocList","jud-ch-bund-bgeList","jud-ch-bund-bguList","jud-ch-bund-bvgeList","jud-ch-bund-bvgerList","jud-ch-bund-tpfList","jud-ch-bund-bstgerList","lex-ch-bund-asList","lex-ch-bund-bblList","lex-ch-bund-abList","jud-ch-ag-agveList"],"size":"100"}})"
      body <- gsub("%fromDate%", fromDate, body)
      body <- gsub("%toDate%", toDate, body)
      json_data <- jsonlite::parse_json(body)

      url <- "https://bstger.weblaw.ch/api/getDocuments?withAggregations=true" # New 2025-12-15
      responseJSON <- httr::POST(url, h, body = json_data, encode = "json")
      responseJSON <- httr::POST(
                            url, h,
                            body = json_data, encode = "json",
                            # Set pseudo Authentication
                            set_cookies(`next-auth.session-token` = cookie)
                            )

      # Check Response
      if(httr::status_code(responseJSON) == "200"){
        json <- httr::content(responseJSON, as="text", encoding = "UTF8")
        df <- jsonlite::parse_json(json)$data
        stopifnot(df$status == "success")

        N <- df$totalNumberOfDocuments %>% as.integer()

        if(N > 0){

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
                          # URL =  unlist(d$metadataKeywordTextMap$originalUrl),
                          FILENAME = unlist(d$metadataKeywordTextMap$fileName),
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
          message(decision$FILENAME)

          # Download to Destination
          fi <- decision$FILENAME  # New 2025-12-15

          yr <- substr(fi, 1,4)
          dest <- gsub("DEST=", "", user$DEST)
          fullname <- file.path(dest, "BStGer/Beschwerdekammer_RH", yr, fi)

          dirname <- dirname(fullname)
          if(!dir.exists(dirname)) dir.create(dirname, recursive = TRUE)

          if(!file.exists(fullname)){
            # tryCatch(
            #   {download.file(pdf_link, fullname, mode="wb")},
            #   error = function(cond) {
            #     message(paste("URL does not seem to exist:", url))
            #     message("Here's the original error message:")
            #     message(conditionMessage(cond))
            #     # Choose a return value in case of error
            #     # NA
            #   }
            #
            # )

            # Where is the Link to PDF File??
            # It does not exist anymore !
            # You have to upload with POST a json payload to the url with the lei
            # Example
            # URL:  https://bstger.weblaw.ch/api/getDocumentFile/494db0c7-ad18-3225-a4e3-5a00046077d1?locale=de&userID=_5g5nenb
            # Payload looks like: {documentTitle: "20250807_RR_2025_109.pdf"}

            save_pdf_icon <- paste0("https://bstger.weblaw.ch/api/getDocumentFile/", d$leid,"?locale=de&userID=_5g5nenb")

            # JSON-Body
            # It is not important to be correct
            # on 2025-12-15 it was working without correct filename
            payload <- list(documentTitle = decision$FILENAME)

            # POST-Request mit JSON
            save_response <- POST(
                              save_pdf_icon,
                              body = payload,
                              encode = "json",
                              # wichtig, damit der Server PDF zurückgibt
                              add_headers(
                                "Content-Type" = "application/json",
                                "Accept" = "application/pdf"
                              ),
                              # falls nötig: Cookies für Session hinzufügen
                              set_cookies(`next-auth.session-token` = cookie)
                            )

            # Save PDF as binary file
            bin <- content(save_response, "raw")
            writeBin(bin, fullname)

          }

        }
        }
      }

    }

    # Data Collection
    if(length(json_archive) == 0){
      result <- NULL
    } else {
      result <- json_archive %>%
              do.call(dplyr::bind_rows, .) %>%
              mutate(
                # URL = paste0(host, URL),
                ENTSCHEIDTYP = gsub("^(.*?);;.*", "\\1", ENTSCHEIDTYP),
                CONTENT = gsub("^(.*?);;.*", "\\1", CONTENT),
                KAMMER = gsub("^(.*?);;.*", "\\1", KAMMER),
                ENTSCHEIDSTATUS = gsub("^(.*?);;.*", "\\1", ENTSCHEIDSTATUS),
                ENTSCHEIDDATUM = as.POSIXct(ENTSCHEIDDATUM),
                PUBLUKATIONSDATUM = as.POSIXct(PUBLUKATIONSDATUM)
              )

      attr(result, "Name") <- "Gerichtsentscheide BStGer"
    }



    # Return
    return(result)
}


# Cookie AI Analysis ####

# Fortinet/WAF-Cookie: FTNT-EP-… sieht nach einem Fortinet Web Application Firewall (WAF) / Endpoint‑Cookie aus. Solche Cookies werden oft als Sitzungs-/Bot‑Schutz gesetzt und können an bestimmte Domäne, Path, und TLS‑Eigenschaften gebunden sein. Fehlen passende Header (User‑Agent, Origin, Referer) oder ist die Anfrage „bot‑artig“, blockt die WAF trotz gültigem Session‑Cookie.
#
# NextAuth CSRF: next-auth.csrf-token wird bei Formular-/OAuth‑Flows benötigt. Bei reinen API‑Calls nach erfolgreichem Login ist dieser Cookie normalerweise nicht erforderlich, es sei denn, du triffst Endpunkte, die explizit den CSRF‑Token erwarten (z. B. Mutations über Webseiten-Routen statt dedizierten API-Routen). Wenn er benötigt wird, muss er konsistent mit dem Request‑Header/Body gesendet werden, sonst scheitert der POST.
#
# Callback-URL auf localhost: next-auth.callback-url=http://localhost:3000 deutet darauf hin, dass der Login‑Flow lokal erzeugt wurde. Ein Session‑Token, der gegen eine andere Domäne/Umgebung verwendet wird, ist in der Praxis wertlos, weil der Server den Token entschlüsselt/validiert mit seinen eigenen Keys und Konfiguration. Unterschiedliche Umgebungen (localhost vs. Produktion) → anderer Key → der Token ist ungültig.
#
# NextAuth JWE-Token: next-auth.session-token=eyJhbGciOiJkaXIiLCJlbmMiOiJBMjU2R0NNIn0… hat Header alg=dir, enc=A256GCM → das ist ein verschlüsseltes JWE. Du kannst ihn nicht „nachbauen“. Er ist serverseitig ausgestellt und oft kurzlebig. Wenn er ausgelaufen ist oder nicht zu Domäne/Secrets passt, bekommst du 401/403 oder Redirect zur Anmeldung.
#
# Cookie-Scope/Flags: Auch wenn du den Wert hast, muss er zur Ziel‑Domäne und zum Path passen. Außerdem können Secure und SameSite verhindern, dass der Cookie akzeptiert wird, wenn du nicht über HTTPS oder mit passendem Origin/Referer arbeitest.
