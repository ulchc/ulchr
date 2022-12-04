#' Get downloads folder
#'
#' Return the path to the downloads directory regardless of operating system.
#'
#' @return A character file path.
#' @export
#' @examples
#' get_downloads_folder()
get_downloads_folder <- function() {
  if (Sys.info()["sysname"] == "Windows")
    folder <- paste0("C:/Users/", Sys.info()["login"])
  else
    folder <- path.expand("~")
  folder <- file.path(folder, "Downloads")
  folder <- paste0(folder, .Platform$file.sep)
  return(folder)
}

