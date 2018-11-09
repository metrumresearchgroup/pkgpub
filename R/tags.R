#' get the tag of the head (current commit)
#' @export
head_tag <- function() {
  sh("git", c("tag", "-l", "--points-at", "HEAD")) %>%
    if_error(character(0))
}

