#' get the tag of the head (current commit)
#' @export
head_tag <- function() {
  sh("git", c("tag", "-l", "--points-at", "HEAD")) %>%
    if_error(character(0))
}

is_dev_tag <- function() {
  ctag <- current_commit_tag()
  # given 0.1.2.9000 the 4 will be 9000
  # given 0.1.2 the 4 will be NA
  dev_version <- ctag[1, 4]
  return(is.na(dev_version))
}
