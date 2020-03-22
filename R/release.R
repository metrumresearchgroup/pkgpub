#' create a cranlike repo that contains the tagged package
#' @param .dir directory to create the repo in
#' @export
create_tagged_repo <- function(.dir = "/tmp") {
  ctag <- current_commit_tag()

  # not tagged
  if (is.null(ctag)) {
   stop("current commit is not tagged")
  }
  desc <- as.data.frame(read.dcf("DESCRIPTION",fields = "Version"))
  if (desc$Version != ctag) {
    stop(sprintf("mismatch between git tag (%s) and DESCRIPTION Version (%s)", ctag, desc$Version), call. = FALSE)
  }

  repo_dir <- new_cranlike_repo(file.path(.dir, ctag))

  built_dir <- build_pkg(.pkgdir = ".", types = "source", origin = sanitize_git_url(git_url()), repository = "MPNDEV", addl_meta = list(git_tag = ctag), supplement_version = FALSE)

  res <- insert_packages(unlist(built_dir), repo_dir)
  if (!isTRUE(res)) {
    stop("failure building or inserting package")
  }
  return(repo_dir)
}
