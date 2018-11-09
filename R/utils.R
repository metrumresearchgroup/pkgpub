drop_nulls <- function(.x) {
  .x[sapply(.x, is.null)] <- NULL
  .x
}

sh <- function(cmd, args, stdout = TRUE, ..., .dir = getwd()) {
  owd <- getwd()
  if (.dir != owd) {
    setwd(.dir)
    on.exit({
      setwd(owd)
    }, add = TRUE)
  }
  tryCatch({
    system2(cmd, args, stdout = stdout, ...)
  }, error = function(e) {
    e
  })
}

if_error <- function(.x, when_error = NULL) {
  if (inherits("simpleError")) {
    return(when_error)
  }
  return(.x)
}
