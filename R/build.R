modify_desc <- function(.d, meta, loc, overwrite = TRUE) {
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
  d__$write(loc)
  return(fields_set)
}


#' build various versions of a package
#' @param .pkgdir path to package
#' @param types types of package to build
#' @param repository repository name being built for
#' @param origin package source
#' @param addl_meta additional metadata
#' @param supplement_version add additional version info (unix timestamp) to version.
#' TRUE inspects the pkg folder as a git repo, also may provide a character or numeric
#' value to append
#' @param overwrite overwrite fields already present when adding fields
#' @param ... parameters to pass to pkgbuild
#' @details
#' supplementing version can be done whenever a build occurs that does not
#' correspond to a formal release/tag. This will automatically add information
#' about the git hash (if available), as well as incrememnt the version number
#' with a unix timestamp that corresponds to the last git hash (if present) or
#' the current system time, if git is not present and no version timestamp is provided.
#' @importFrom stats setNames
#' @importFrom utils modifyList
#' @export
build_pkg <- function(.pkgdir = ".",
                      types = c("source", "binary"),
                       repository = NULL,
                       origin = NULL,
                       addl_meta = NULL,
                       supplement_version = FALSE,
                       overwrite = TRUE,
                      ...) {
  withr::with_dir(.pkgdir, {
    pkg_desc <- file.path(.pkgdir, "DESCRIPTION")
    d__ <- desc::desc(pkg_desc)
    meta <- list(Repository = repository, Origin = origin)
    if (!is.null(addl_meta)) {
      meta <- modifyList(meta, addl_meta)
    }
    if (is.numeric(supplement_version) || is.character(supplement_version)) {
      version <- d__$get_version()
      sv <- list(
        timestamp = as.integer(supplement_version),
        Version = sprintf("%s.%s", version, supplement_version)
      )
      meta <- modifyList(meta, sv)
    } else if (supplement_version) {
      hs <- hashstamp()
      version <- d__$get_version()
      hs$Version <- sprintf("%s.%s", version, hs$timestamp)
      meta <- modifyList(meta, hs)
    }
    modify_desc(d__, drop_nulls(meta), loc = pkg_desc, overwrite = overwrite)
    on.exit({
      # d__ should be unchanged
      d__$write(pkg_desc)
    }, add = TRUE)
    result <- tryCatch({
      tballs <- lapply(types, function(.t) {
        switch(.t,
               source = pkgbuild::build(.pkgdir, ...),
               binary = pkgbuild::build(.pkgdir, binary = TRUE, ...),
               stop(sprintf("cannot build type %s", .t)))
      })
      setNames(tballs, types)
    }, error = function(e) {
      message("building failed", e)
      e
    })
    return(result)
  })
}
