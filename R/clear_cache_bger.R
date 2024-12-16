#' Delete Cache from Download BGer
#'
#' Clear Cache for last page for complete download next time
#'
#' @param dest
#'
#' @return Clean Chache
#' @export
#'
#' @examples
#' clear_cache_bger()
#' clear_cache_bger(2019)
clear_cache_bger <- function(year = get_year(),
                             lastPage = TRUE,
                             dest = NA){

  # Destination
  if(is.na(dest)){
    dest <- read_options() %>% filter(KEY == "DEST") %>% pull()
    dest <- file.path(dest, "BGer_WeitereUrteile", ".cache")
  }

  # Read Files and take only from year x
  fls <- list.files(dest,full.names = TRUE)
  idx <- grep(year,fls)
  fls <- fls[idx]

  # Split up text by path seperator
  m <- strsplit(fls, split="/")
  m <- sapply(m, function(x) tail(x,1))

  # Get numbers from string
  y <- sapply(m, function(x) strsplit(x,split = "-")[[1]][3])
  nms <- names(y)
  y <- gsub("\\(","",y)
  y <- gsub("\\).txt","",y)
  y <- as.numeric(y)
  num <- y

  # Get from an to dates
  y1 <- sapply(m, function(x) strsplit(x,split = "-")[[1]][1])
  y2 <- sapply(m, function(x) strsplit(x,split = "-")[[1]][2])

  # Span data.frame
  df <- data.frame(from = y1,
                   to = y2,
                   fromto = paste(y1,y2,sep = "-"),
                   index = num,
                   fls = fls,
                   stringsAsFactors = FALSE)

  # Create proper order
  idx <- order(df$from,df$to,-df$index)
  df <- df[idx,]

  # Find last
  if (lastPage){
    df$LastPage <- !duplicated(df$fromto)
    result <- df$fls[which(df$LastPage)]
  } else {
    result <- df$fls
  }

  # Delete Files
  all(file.remove(result))
  message("Deleted files:");print(result)
}
