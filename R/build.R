modify_desc <- function(.d, meta, overwrite = TRUE) {
  if (!inherits(.d, "description")) {
    stop("must pass a desc object to modify_desc")
  }
  d__ <- .d$clone()
  fields_set <- lapply(names(meta), function(.f) {
    if (!overwrite && !is.na(d__$get(.f))) {
      return(NULL)
    }
    d__$set(.f, meta[[.f]])
    return(.f)
  })
  if (is.na(d__$get("Repository"))) {
    stop("package must have a `Repository` field set", call. = FALSE)
  }
  d__$write(pkg_desc)
  return(fields_set)
}


#' build various versions of a package
#' @param .pkg path to package
#' @param repository repository name being built for
#' @param origin package source
#' @param supplement_version whether to add additional version info (unix timestamp)
#' @details
#' supplementing version can be done whenever a build occurs that does not
#' correspond to a formal release/tag. This will automatically add information
#' about the git hash (if available), as well as incrememnt the version number
#' with a unix timestamp that corresponds to the last git hash (if present) or
#' the current system time, if git is not present.
#' @export
build_pkg <- function(.pkg = ".",
                      types = c("source", "binary"),
                       repository = NULL,
                       origin = NULL,
                       supplement_version = FALSE) {
  pkg_desc <- file.path(.pkg, "DESCRIPTION")
  d__ <- desc::desc(pkg_desc)
  meta <- list(Repository = repository, Origin = origin)
  if (supplement_version) {
    hs <- hashstamp(.pkg)
    version <- d__$get_version()
    hs$Version <- sprintf("%s.%s", version, hs$timestamp)
    meta <- modifyList(meta, hs)
  }
  modify_desc(d__, drop_nulls(meta))
  on.exit({
    # d__ should be unchanged
    d__$write(pkg_desc)
  }, add = TRUE)
  result <- tryCatch({
    balls <- lapply(types, function(.t) {
     switch(.t,
            source = pkgbuild::build(.pkg),
            binary = pkgbuild::build(.pkg, binary = TRUE),
            stop(sprintf("cannot build type %s", .t)))
    })
  }, error = function(e) {
    message("building failed", e)
    e
  })

  return(result)
}