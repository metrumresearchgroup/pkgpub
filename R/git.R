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
  tags <- gert::git_tag_list()
  # no tags in repo period
  if (!nrow(tags)) {
    return(NULL)
  }
  ginfo <- gert::git_info()
  tag_df <- tags[tags$commit == ginfo$commit, , drop = FALSE]
  if (!nrow(tag_df)) {
    return(NULL)
  }
  return(tag_df$name)
}
