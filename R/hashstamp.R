hashstamp <- function(path) {
  owd <- getwd()
  on.exit({
    setwd(owd)
  }, add = TRUE)
  setwd(path)
  metadata <- tryCatch({
    timestamp <- system2("git", args = c("show", "-s", "--format=%ct"))
    hash <- system2("git", args = c("rev-parse", "HEAD"))
  }, function(e) {
    warning("no git detected on system, using system time for timestamp")
    return(timestamp = as.numeric(Sys.time()))
  })
  return(metadata)
}
