#' Render and publish a Quarto manuscript to Google Drive
#'
#' Renders a `.qmd` file to DOCX using the bundled reference document and Lua
#' filter, then uploads it to Google Drive. On first publish, a `gdrive` entry
#' for the file is added to `_publish.yml` next to the `.qmd`. This is the
#' same publish-record file `quarto publish` uses, so a doc already deployed
#' to e.g. Posit Connect keeps that record side by side. Commit this file so
#' collaborators always open the same shared document.
#'
#' @param qmd_file Path to the `.qmd` source file.
#' @param no_render If `TRUE`, skip the render step and upload the existing
#'   DOCX next to the `.qmd`.
#' @param quarto_args Character vector of extra arguments passed to
#'   `quarto render`.
#'
#' @export
publish <- function(qmd_file, no_render = FALSE, quarto_args = character()) {
  qmd_file <- check_qmd_file(qmd_file)
  base <- fs::path_file(qmd_file)
  deployments_file <- deployments_file_for(qmd_file)

  check_package("googledrive")
  drive_login()

  docx_file <- if (no_render) {
    fs::path_ext_set(qmd_file, "docx")
  } else {
    render_docx(qmd_file, quarto_args = quarto_args)
  }

  deployments <- load_deployments(deployments_file)
  existing_id <- gdrive_entry(deployments, base)[["id"]]
  doc_id <- upload_to_gdrive(docx_file, fs::path_ext_remove(base), existing_id)
  deployments <- set_gdrive_deployment(deployments, base, doc_id)
  yaml::write_yaml(deployments, deployments_file)

  cli::cli_alert_success("Published: {gdrive_url(doc_id)}")
  if (is.null(existing_id)) {
    cli::cli_alert_info(
      "Commit {.file {deployments_file}} so collaborators point at the same doc."
    )
  }
  invisible(doc_id)
}

#' Open a published manuscript in the browser
#'
#' Reads the document ID from the `gdrive` entry in `_publish.yml` next to the
#' `.qmd` file and opens the Google Doc in the system browser.
#'
#' @param qmd_file Path to the `.qmd` source file.
#'
#' @export
open_published <- function(qmd_file) {
  qmd_file <- check_qmd_file(qmd_file)
  base <- fs::path_file(qmd_file)
  deployments <- load_deployments(deployments_file_for(qmd_file))
  id <- gdrive_entry(deployments, base)[["id"]]
  if (is.null(id)) {
    cli::cli_abort(c(
      "No published doc found for {.file {base}}.",
      "i" = "Run {.run pubthis::publish('{qmd_file}')} first."
    ))
  }
  browse_url(gdrive_url(id))
}

check_qmd_file <- function(qmd_file) {
  if (
    !is.character(qmd_file) ||
      length(qmd_file) != 1 ||
      is.na(qmd_file) ||
      !nzchar(qmd_file)
  ) {
    cli::cli_abort(
      "{.arg qmd_file} must be a single non-empty path to a {.code .qmd} file."
    )
  }
  qmd_file <- fs::path_abs(qmd_file)
  if (!fs::is_file(qmd_file)) {
    cli::cli_abort(c("File not found:", "x" = "{.file {qmd_file}}"))
  }
  ext <- fs::path_ext(qmd_file)
  if (tolower(ext) != "qmd") {
    cli::cli_abort(
      "{.arg qmd_file} must be a {.code .qmd} file, not {.code .{ext}}."
    )
  }
  qmd_file
}

deployments_file_for <- function(qmd_file) {
  fs::path(fs::path_dir(qmd_file), "_publish.yml")
}

drive_login <- function() {
  tryCatch(
    googledrive::drive_auth(email = TRUE),
    error = function(e) {
      cli::cli_abort(
        c(
          "Google Drive authentication failed.",
          "i" = "Run {.code googledrive::drive_auth()} once in an interactive R session to cache credentials."
        ),
        parent = e,
        call = NULL
      )
    }
  )
}

render_docx <- function(qmd_file, quarto_args = character()) {
  wd <- fs::path_dir(qmd_file)
  result <- processx::run(
    "quarto",
    c(
      "render",
      fs::path_file(qmd_file),
      "--to",
      "docx",
      docx_publish_args(qmd_file),
      quarto_args
    ),
    wd = wd,
    stdout = "|",
    stderr = "|",
    error_on_status = FALSE
  )
  if (result$status != 0) {
    render_error <- result$stderr
    cli::cli_abort(c(
      "quarto render failed for {.file {qmd_file}}:",
      "x" = "{render_error}"
    ))
  }
  rendered_output(result, wd = wd, default = fs::path_ext_set(qmd_file, "docx"))
}

# Use Quarto's reported output path when available.
rendered_output <- function(result, wd, default) {
  lines <- unlist(strsplit(c(result$stdout, result$stderr), "\n"))
  created <- grep("^\\s*Output created: ", lines, value = TRUE)
  if (length(created) == 0) {
    return(default)
  }
  path <- trimws(sub("^\\s*Output created: ", "", created[[1]]))
  fs::path_abs(path, start = wd)
}

docx_publish_args <- function(qmd_file) {
  publish_dir <- find_publish_dir(fs::path_dir(qmd_file))
  reference_doc <- fs::path(publish_dir, "reference.docx")
  lua_filter <- fs::path(publish_dir, "docx-format.lua")
  files <- c(reference_doc, lua_filter)
  missing <- files[!fs::file_exists(files)]
  if (length(missing) > 0) {
    cli::cli_abort(c(
      "Missing DOCX publish support file(s).",
      "x" = "{missing}",
      "i" = "Run {.run pubthis::use_publish_workflow()} to add them."
    ))
  }
  c(
    paste0("--reference-doc=", reference_doc),
    paste0("--lua-filter=", lua_filter)
  )
}

find_publish_dir <- function(start_dir) {
  dir <- start_dir
  repeat {
    candidate <- fs::path(dir, "publish")
    if (fs::dir_exists(candidate)) {
      return(candidate)
    }
    parent <- fs::path_dir(dir)
    if (parent == dir) {
      cli::cli_abort(c(
        "No {.file publish/} directory found in {.file {start_dir}} or any parent directory.",
        "i" = "Run {.run pubthis::use_publish_workflow()} in your project to add it."
      ))
    }
    dir <- parent
  }
}

upload_to_gdrive <- function(docx_file, doc_name, existing_id = NULL) {
  if (!fs::file_exists(docx_file)) {
    cli::cli_abort(c("Rendered DOCX not found:", "x" = "{.file {docx_file}}"))
  }
  if (!is.null(existing_id)) {
    confirm_unresolved_comments(existing_id)
    googledrive::drive_update(
      googledrive::as_id(existing_id),
      media = docx_file
    )
    existing_id
  } else {
    result <- googledrive::drive_upload(
      docx_file,
      name = doc_name,
      type = "document"
    )
    as.character(result$id)
  }
}

# Replacing a document can detach existing comments from the text.
confirm_unresolved_comments <- function(doc_id) {
  n <- tryCatch(
    count_unresolved_comments(doc_id),
    error = function(e) {
      cli::cli_alert_danger(
        "Could not check for unresolved comments on {.url {gdrive_url(doc_id)}}: {conditionMessage(e)}"
      )
      NA_integer_
    }
  )
  if (is.na(n) || n == 0) {
    return(invisible())
  }
  cli::cli_alert_warning(
    "{n} unresolved comment{?s} on {.url {gdrive_url(doc_id)}} will lose their position in this republish."
  )
  if (interactive() && !isTRUE(utils::askYesNo("Replace the doc anyway?"))) {
    cli::cli_abort("Publish cancelled.", call = NULL)
  }
  invisible()
}

count_unresolved_comments <- function(doc_id) {
  req <- googledrive::request_generate(
    "drive.comments.list",
    params = list(
      fileId = doc_id,
      fields = "comments(resolved)",
      pageSize = 100
    )
  )
  resp <- gargle::response_process(googledrive::request_make(req))
  comments <- resp$comments
  sum(!vapply(comments, function(x) isTRUE(x$resolved), logical(1)))
}

load_deployments <- function(deployments_file) {
  if (!fs::file_exists(deployments_file)) {
    return(list())
  }
  deployments <- yaml::read_yaml(deployments_file)
  if (is.null(deployments)) {
    return(list())
  }
  if (!is_deployments_list(deployments)) {
    cli::cli_abort(c(
      "{.file {deployments_file}} is not in the expected format.",
      "i" = "It should be a list of entries like {.code - source: file.qmd}."
    ))
  }
  deployments
}

is_deployments_list <- function(x) {
  is.list(x) && (length(x) == 0 || is.null(names(x)))
}

find_source_idx <- function(deployments, base) {
  idx <- which(vapply(
    deployments,
    function(d) identical(d[["source"]], base),
    logical(1)
  ))
  if (length(idx) == 0) NA_integer_ else idx[[1]]
}

gdrive_entry <- function(deployments, base) {
  idx <- find_source_idx(deployments, base)
  if (is.na(idx)) {
    return(NULL)
  }
  gdrive <- deployments[[idx]][["gdrive"]]
  if (is.null(gdrive) || length(gdrive) == 0) {
    return(NULL)
  }
  gdrive[[1]]
}

set_gdrive_deployment <- function(deployments, base, doc_id) {
  record <- list(id = doc_id, url = gdrive_url(doc_id))
  idx <- find_source_idx(deployments, base)
  if (is.na(idx)) {
    deployments[[length(deployments) + 1]] <- list(
      source = base,
      gdrive = list(record)
    )
  } else {
    deployments[[idx]][["gdrive"]] <- list(record)
  }
  deployments
}

gdrive_url <- function(id) {
  paste0("https://docs.google.com/document/d/", id)
}

browse_url <- function(url) {
  utils::browseURL(url)
}

check_package <- function(pkg) {
  if (!requireNamespace(pkg, quietly = TRUE)) {
    cli::cli_abort(c(
      "Package {.pkg {pkg}} is required but not installed.",
      "i" = "Install with {.run install.packages('{pkg}')}."
    ))
  }
}
