#' Render and publish a Quarto manuscript to Google Drive
#'
#' Renders a `.qmd` file to DOCX using the bundled reference document and Lua
#' filter, then uploads it to Google Drive. On first publish a
#' `_publish_ids.yml` file is created next to the `.qmd` — commit this so
#' collaborators always open the same shared document.
#'
#' @param qmd_file Path to the `.qmd` source file.
#' @param no_render If `TRUE`, skip the render step and upload the existing
#'   DOCX.
#' @param quarto_args Character vector of extra arguments passed to
#'   `quarto render`.
#'
#' @export
publish <- function(qmd_file, no_render = FALSE, quarto_args = character()) {
  publish_gdrive(qmd_file, no_render = no_render, quarto_args = quarto_args)
}

#' Open a published manuscript in the browser
#'
#' Reads the URL from `_publish_ids.yml` next to the `.qmd` file and opens it
#' in the system browser.
#'
#' @param qmd_file Path to the `.qmd` source file.
#'
#' @export
open_published <- function(qmd_file) {
  qmd_file <- fs::path_abs(qmd_file)
  base <- fs::path_file(qmd_file)
  ids_file <- fs::path(fs::path_dir(qmd_file), "_publish_ids.yml")
  url <- load_ids(ids_file)[["gdrive"]][[base]][["url"]]
  if (is.null(url)) {
    cli::cli_abort(c(
      "No published URL found for {.file {base}}.",
      "i" = "Run {.run pubthis::publish('{qmd_file}')} first."
    ))
  }
  utils::browseURL(url)
}

publish_gdrive <- function(qmd_file, no_render = FALSE, quarto_args = character()) {
  qmd_file <- fs::path_abs(qmd_file)
  if (!fs::file_exists(qmd_file)) {
    cli::cli_abort(c("File not found:", "x" = "{.file {qmd_file}}"))
  }

  check_package("googledrive")
  options(gargle_oauth_email = TRUE)
  googledrive::drive_auth()
  if (is.null(googledrive::drive_user())) {
    cli::cli_abort(c(
      "Not authenticated with Google Drive.",
      "i" = "Run {.code just auth-gdrive} for instructions."
    ), call = NULL)
  }

  run_publish(qmd_file, no_render = no_render, quarto_args = quarto_args)
}

# Shared pipeline: resolve paths, render, upload, track IDs, emit messages.
run_publish <- function(qmd_file, no_render, quarto_args) {
  base <- fs::path_file(qmd_file)
  docx_file <- fs::path_ext_set(qmd_file, "docx")
  doc_name <- fs::path_ext_remove(base)
  ids_file <- fs::path(fs::path_dir(qmd_file), "_publish_ids.yml")

  if (!no_render) render_docx(qmd_file, quarto_args = quarto_args)

  all_ids <- load_ids(ids_file)
  is_new <- is.null(all_ids[["gdrive"]]) || !(base %in% names(all_ids[["gdrive"]]))

  doc_id <- upload_to_gdrive(docx_file, doc_name, all_ids[["gdrive"]][[base]][["id"]])
  all_ids[["gdrive"]][[base]] <- list(
    id = doc_id,
    url = gdrive_url(doc_id),
    last_published = now_utc()
  )
  yaml::write_yaml(all_ids, ids_file)

  cli::cli_alert_success("Published: {all_ids[['gdrive']][[base]][['url']]}")
  if (is_new) cli::cli_alert_info("Commit {.file {ids_file}} so collaborators point at the same doc.")
}

render_docx <- function(qmd_file, quarto_args = character()) {
  qmd_file <- fs::path_abs(qmd_file)
  result <- processx::run(
    "quarto", c(
      "render", fs::path_file(qmd_file), "--to", "docx",
      docx_publish_args(), quarto_args
    ),
    wd = fs::path_dir(qmd_file),
    stdout = "|", stderr = "|",
    error_on_status = FALSE
  )
  if (result$status != 0) {
    cli::cli_abort(c("quarto render failed for {.file {qmd_file}}:", result$stderr))
  }
  invisible(result)
}

docx_publish_args <- function() {
  reference_doc <- here::here("publish/reference.docx")
  lua_filter <- here::here("publish/docx-format.lua")
  missing <- c(reference_doc, lua_filter)[!fs::file_exists(c(reference_doc, lua_filter))]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing DOCX publish support file(s).",
      "x" = "{missing}",
      "i" = "Run {.run pubthis::use_publish_workflow()} to add them."
    ))
  }
  c(paste0("--reference-doc=", reference_doc), paste0("--lua-filter=", lua_filter))
}

upload_to_gdrive <- function(docx_file, doc_name, existing_id = NULL) {
  if (!fs::file_exists(docx_file)) {
    cli::cli_abort(c("Rendered DOCX not found:", "x" = "{.file {docx_file}}"))
  }
  if (!is.null(existing_id)) {
    googledrive::drive_update(googledrive::as_id(existing_id), media = docx_file)
    existing_id
  } else {
    result <- googledrive::drive_upload(docx_file, name = doc_name, type = "document")
    as.character(result$id)
  }
}

load_ids <- function(ids_file) {
  if (!fs::file_exists(ids_file)) return(list())
  ids <- yaml::read_yaml(ids_file)
  if (is.null(ids)) list() else ids
}

gdrive_url <- function(id) {
  paste0("https://docs.google.com/document/d/", id)
}

now_utc <- function() {
  format(as.POSIXct(Sys.time(), tz = "UTC"), "%Y-%m-%dT%H:%M:%SZ")
}

check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required but not installed.",
      "i" = "Install with {.run install.packages('{pkg}')}."
    ))
  }
}
