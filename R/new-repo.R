#' new cranlike repo will create a cranlike repo
#' @param dir directory to create
#' @export
new_cranlike_repo <- function(dir = getwd()) {
  rp <- file.path(dir, "src", "contrib")
  if (!dir.exists(rp)) {
    dir.create(rp, recursive = TRUE)
  }
  return(dir)
}
