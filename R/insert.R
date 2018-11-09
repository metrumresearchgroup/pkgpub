#' insert packages into a drat repo
#' @param pkgs list of packages to insert
#' @param repository repository folder to insert
#' @details
#' repository folder should most likely correspond to the name
#' of the repository set in the pkg metadata, however
#' realistically, the time that they need to match when
#' repos is set in a user session in the `getOption('repos')` field
#' @export
insert_packages <- function(pkgs, repository) {
  if (!dir.exists(repository)) {
    stop("no directory exists at ", repository, call. = FALSE)
  }
  added_pkgs <- lapply(pkgs, function(.p) {
    if (!file.exists(.p)) {
      warning("no package exists at ", .p, call. = FALSE)
      return(FALSE)
    }
    drat::insertPackage(.p, repodir = repository)
    return(TRUE)
  })
  message(sprintf("successfully added packages:\n %s", paste0(basename(pkgs[unlist(added_pkgs)]), collapse = ",\n ")))
  drat::archivePackages(repopath = repository)
  return(unlist(added_pkgs))
}
