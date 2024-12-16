#' Read Options from local user file
#'
#' In order to efficiently use options some variables are stored in a
#' KEY-VALUE-Pair file in the users local document folder with name `.courtcase`.
#' Please use the format and be ware to use "/" as path separator:
#' DEST=PathToDestination
#' SB=PathToFileWithSBs
#'
#' @param path
#' @param file
#' @param split
#'
#' @return
#' @export
#'
#' @examples
#' read_options()
#' read_options() %>% filter(KEY == "SB") %>% pull()
#' read_options() %>% filter(KEY == "DEST") %>% pull()
read_options <- function(path = Sys.getenv("R_USER"),
                         file = ".courtcase",
                         split = "="){
  # Read text file with KeyValue Pairs
  opts <- readLines(file.path(path, file), warn = FALSE)
  opts <- do.call(rbind.data.frame, strsplit(opts, split = split))
  names(opts) <- c("KEY", "VALUE")
  # Return
  tibble(opts)
}
