GIT <- function(..., .err = NULL) {
  sh("git", args = list(...)) %>% if_error(.err)
}

#' get the origin url
#' @param remote remote name
#' @export
git_url <- function(remote = "origin") {
  GIT("remote", "get-url", remote)
}

# turn git or https
# should result in a canonical URL
# github.com/org/repo
# dropping either git or http notation
sanitize_git_url <- function(.x) {
  .x <- gsub("https://", "", .x)
  .x <- gsub("git@", "", .x)
  .x <- gsub("\\.git$", "", .x)
  .x <- gsub(":", "/", .x)
  return(.x)
}

current_commit_tag <- function() {
  tag <- GIT("tag", "--points-at", "HEAD")
  if (length(tag) > 1) {
    message('detected more than one tag, removing possible `latest` before proceeding')
    tag <- tag[tag != "latest"]
  }
  return(tag)
}
