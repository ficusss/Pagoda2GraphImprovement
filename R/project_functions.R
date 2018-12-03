#' @export
DataPath <- function(path) rprojroot::is_r_package$find_file(paste0("data/", path))
