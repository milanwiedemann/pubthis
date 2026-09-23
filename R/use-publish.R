#' Set up a publication workflow in the current project
#'
#' Copies `publish/reference.docx` and `publish/docx-format.lua` into the
#' active project. Optionally also copies a `justfile` for terminal-based
#' publishing. Existing files are checked but never replaced.
#'
#' @param justfile Copy the `justfile` into the project? The default is
#'   `FALSE`.
#'
#' @export
use_publish_workflow <- function(justfile = FALSE) {
  copy_template(
    "docx-format.lua",
    save_as = fs::path("publish", "docx-format.lua")
  )
  copy_template(
    "reference.docx",
    save_as = fs::path("publish", "reference.docx")
  )
  has_justfile <- fs::file_exists(fs::path(usethis::proj_get(), "justfile"))
  if (justfile || has_justfile) {
    copy_template("justfile", save_as = "justfile")
  }
  if (justfile) {
    cli::cli_inform(c(
      "i" = "The {.file justfile} needs {.href [just](https://github.com/casey/just)} (e.g. {.code brew install just}).",
      "i" = "Run {.code just} in your terminal to see available commands."
    ))
  }
}

copy_template <- function(template, save_as) {
  src <- system.file(
    "templates",
    template,
    package = "pubthis",
    mustWork = TRUE
  )
  dest <- fs::path(usethis::proj_get(), save_as)
  if (fs::file_exists(dest)) {
    if (same_contents(src, dest)) {
      cli::cli_inform(c(
        "i" = "{.file {save_as}} already exists and matches the current template, skipping."
      ))
    } else {
      cli::cli_inform(c(
        "!" = "{.file {save_as}} already exists but differs from the current template.",
        "i" = "Delete it and re-run {.run pubthis::use_publish_workflow()} to update, or keep your customised version."
      ))
    }
    return(invisible(dest))
  }
  fs::dir_create(fs::path_dir(dest))
  fs::file_copy(src, dest)
  cli::cli_inform(c("v" = "Writing {.file {save_as}}."))
  invisible(dest)
}

same_contents <- function(a, b) {
  identical(
    readBin(a, "raw", n = fs::file_size(a)),
    readBin(b, "raw", n = fs::file_size(b))
  )
}
