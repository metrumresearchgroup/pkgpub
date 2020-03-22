#' add a git tag based on the pkg version
#' @param .dir dir of repo
#' @param .dirty whether to tag given the repo is in a dirty state
#' @param .fetch run a git fetch first to get tags
#' @export
tag_version <- function(.dir = getwd(), .dirty = FALSE, .fetch = TRUE) {
  if (.fetch) {
    gert::git_fetch(verbose = FALSE)

  }
  pkg_version <- as.data.frame(read.dcf(file.path(.dir,"DESCRIPTION"),fields = "Version"))$Version
  status <- gert::git_status()
  if (nrow(status) && !.dirty) {
    stop("not tagging as currently detected the following dirty files: ",
         glue::glue_collapse(status$file),
         call. = FALSE
    )
  }
  tag_list <- gert::git_tag_list()
  if (nrow(tag_list) && pkg_version %in% tag_list$name) {
    stop("tag already exists in repo",call. = FALSE)
  }
  gert::git_tag_create(pkg_version, message = sprintf("release v%s", pkg_version))
}
