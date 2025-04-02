
#' @keywords internal
"_PACKAGE"

#' @import helpr
NULL

# on load, make all the package objects
# available in the namespace for internal use
.onLoad <- function(...) {
  # the objects passed here must match the *.rda files
  # inside the `data/` directory

  # this is to register the internal S3 methods
  # this avoids having to export the methods in the NAMESPACE file
  register_s3_method("featureselectr", ".stripLMC", "default")
  register_s3_method("featureselectr", ".stripLMC", "glm")
  register_s3_method("featureselectr", ".stripLMC", "lm")
  register_s3_method("featureselectr", ".fit_nb", "default")
  register_s3_method("featureselectr", ".fit_nb", "formula")
  register_s3_method("featureselectr", ".cost", "fs_auc")
  register_s3_method("featureselectr", ".cost", "fs_ccc")
  register_s3_method("featureselectr", ".cost", "fs_sens_spec")
  register_s3_method("featureselectr", ".cost", "fs_mse")
  register_s3_method("featureselectr", ".cost", "fs_r2")
}



# this wrapper registers the methods during pkg load
# but ensures the package passes R CMD check that it can
# be installed even though pillar isn't imported
register_s3_method <- function(pkg, generic, class, fun = NULL) {
  stopifnot(
    is.character(pkg), length(pkg) == 1L,
    is.character(generic), length(generic) == 1L,
    is.character(class), length(class) == 1L
  )

  if ( is.null(fun) ) {
    fun <- get(paste0(generic, ".", class), envir = parent.frame())
  } else {
    stopifnot(is.function(fun))
  }

  if ( pkg %in% loadedNamespaces() ) {
    registerS3method(generic, class, fun, envir = asNamespace(pkg))
  }

  # Always register hook in case package is later unloaded & reloaded
  setHook(
    packageEvent(pkg, "onLoad"),
    function(...) {
      registerS3method(generic, class, fun, envir = asNamespace(pkg))
    }
  )
}
