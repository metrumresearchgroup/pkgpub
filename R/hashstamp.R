hashstamp <- function() {
  metadata <- tryCatch({
    timestamp <- as.numeric(system2("git", args = c("show", "-s", "--format=%ct"), stdout = TRUE))
    hash <- system2("git", args = c("rev-parse", "HEAD"), stdout = TRUE)
    list(timestamp = timestamp, hash = hash)
  }, error = function(e) {
    warning("no git detected on system, using system time for timestamp")
    return(list(timestamp = as.numeric(Sys.time())))
  })
  return(metadata)
}
