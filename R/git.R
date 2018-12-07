GIT <- function(..., .err = NULL) {
  sh("git", args = list(...)) %>% if_error(.err)
}

#' get the origin url
#' @param remote remote name
#' @export
git_url <- function(remote = "origin") {
  GIT("remote", "get-url", remote)
}

sanitize_ssh <- function(.x) {
  TODO()
}
