drop_nulls <- function(.x) {
  .x[sapply(.x, is.null)] <- NULL
  .x
}
