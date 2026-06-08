#' Set up a publication workflow in the current project
#'
#' Copies `publish/reference.docx` and `publish/docx-format.lua` into the
#' active project. Optionally also copies a `justfile` for terminal-based
#' publishing and prints installation instructions for `just`.
#'
#' @param justfile Copy the `justfile` into the project? Set to `FALSE` if you
#'   prefer to call `pubthis::publish()` directly from R without using `just`.
#'
#' @export
use_publish_workflow <- function(justfile = TRUE) {
  copy_template("docx-format.lua", save_as = file.path("publish", "docx-format.lua"))
  copy_template("reference.docx", save_as = file.path("publish", "reference.docx"))
  if (justfile) {
    usethis::use_template("justfile", package = "pubthis")
    use_just_instructions()
  }
}

copy_template <- function(template, save_as) {
  src <- system.file("templates", template, package = "pubthis", mustWork = TRUE)
  dest <- fs::path(usethis::proj_get(), save_as)
  fs::dir_create(fs::path_dir(dest))
  if (fs::file_exists(dest)) {
    cli::cli_inform(c("i" = "{.file {save_as}} already exists, skipping."))
    return(invisible(dest))
  }
  fs::file_copy(src, dest)
  cli::cli_inform(c("v" = "Writing {.file {save_as}}."))
  invisible(dest)
}

#' Print instructions for installing just
#'
#' @export
use_just_instructions <- function() {
  cli::cli_inform(c(
    "",
    "i" = "This workflow requires {.href [just](https://github.com/casey/just)}.",
    "",
    "Install on macOS:",
    " " = "{.code brew install just}",
    "",
    "Install on Windows:",
    " " = "{.code winget install Casey.Just}",
    " " = "or {.code scoop install just}",
    "",
    "Install on Linux:",
    " " = "{.code cargo install just}",
    " " = "or see {.url https://github.com/casey/just#packages} for your distro.",
    "",
    "i" = "After installing, run {.code just} in your terminal to see available commands."
  ))
}
