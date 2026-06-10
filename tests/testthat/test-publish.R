# --- input validation -------------------------------------------------------

test_that("publish rejects an empty path before touching anything", {
  # Regression: a broken justfile once ran pubthis::publish('') and the error
  # surfaced deep in the upload step as "Rendered DOCX not found".
  expect_error(publish(""), "must be a single non-empty")
})

test_that("check_qmd_file rejects non-scalar and missing input", {
  expect_error(check_qmd_file(""), "must be a single non-empty")
  expect_error(check_qmd_file(NULL), "must be a single non-empty")
  expect_error(check_qmd_file(NA_character_), "must be a single non-empty")
  expect_error(check_qmd_file(c("a.qmd", "b.qmd")), "must be a single non-empty")
  expect_error(check_qmd_file(tempfile(fileext = ".qmd")), "File not found")
})

test_that("check_qmd_file rejects directories", {
  # fs::file_exists() is TRUE for directories, which let publish('') through.
  tmp <- withr::local_tempdir()
  expect_error(check_qmd_file(tmp), "File not found")
})

test_that("check_qmd_file rejects non-qmd files", {
  f <- withr::local_tempfile(fileext = ".docx")
  file.create(f)
  expect_error(check_qmd_file(f), "must be a")
})

test_that("check_qmd_file returns the absolute path for a valid qmd", {
  tmp <- withr::local_tempdir()
  qmd <- file.path(tmp, "paper.qmd")
  file.create(qmd)
  withr::local_dir(tmp)
  result <- check_qmd_file("paper.qmd")
  expect_true(fs::is_absolute_path(result))
  expect_equal(fs::path_file(result), "paper.qmd")
})

# --- publish ids ------------------------------------------------------------

test_that("load_ids returns empty list when file does not exist", {
  expect_equal(load_ids(tempfile()), list())
})

test_that("load_ids returns empty list for empty yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  writeLines("", f)
  expect_equal(load_ids(f), list())
})

test_that("load_ids round-trips yaml", {
  f <- withr::local_tempfile(fileext = ".yml")
  ids <- list(gdrive = list("paper.qmd" = list(id = "abc123")))
  yaml::write_yaml(ids, f)
  expect_equal(load_ids(f), ids)
})

test_that("gdrive_url builds correct URL", {
  expect_equal(gdrive_url("abc123"), "https://docs.google.com/document/d/abc123")
})

# --- open_published ---------------------------------------------------------

test_that("open_published errors when no ids file exists", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  expect_error(open_published(file.path(tmp, "paper.qmd")), "No published doc found")
})

test_that("open_published errors when gdrive entry is missing", {
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(gdrive = list("other.qmd" = list(id = "abc123"))),
    file.path(tmp, "_publish_ids.yml")
  )
  expect_error(open_published(file.path(tmp, "paper.qmd")), "No published doc found")
})

test_that("open_published derives the URL from a hand-created id-only yml", {
  # The README documents creating _publish_ids.yml with only an id key.
  tmp <- withr::local_tempdir()
  file.create(file.path(tmp, "paper.qmd"))
  yaml::write_yaml(
    list(gdrive = list("paper.qmd" = list(id = "abc123"))),
    file.path(tmp, "_publish_ids.yml")
  )
  opened <- NULL
  local_mocked_bindings(browse_url = function(url) opened <<- url)
  open_published(file.path(tmp, "paper.qmd"))
  expect_equal(opened, "https://docs.google.com/document/d/abc123")
})

# --- publish support files --------------------------------------------------

test_that("find_publish_dir finds publish/ next to the qmd", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  expect_equal(find_publish_dir(fs::path(tmp)), fs::path(tmp, "publish"))
})

test_that("find_publish_dir walks up from a manuscripts/ subdirectory", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  fs::dir_create(fs::path(tmp, "manuscripts"))
  expect_equal(find_publish_dir(fs::path(tmp, "manuscripts")), fs::path(tmp, "publish"))
})

test_that("find_publish_dir errors when no publish/ exists in any parent", {
  tmp <- withr::local_tempdir()
  expect_error(find_publish_dir(fs::path(tmp)), "No .*publish.* directory found")
})

test_that("docx_publish_args errors when support files are missing", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  expect_error(
    docx_publish_args(fs::path(tmp, "paper.qmd")),
    "Missing DOCX publish support file"
  )
})

test_that("docx_publish_args resolves files relative to the qmd, not getwd()", {
  tmp <- withr::local_tempdir()
  fs::dir_create(fs::path(tmp, "publish"))
  fs::dir_create(fs::path(tmp, "manuscripts"))
  file.create(fs::path(tmp, "publish", "reference.docx"))
  file.create(fs::path(tmp, "publish", "docx-format.lua"))
  withr::local_dir(withr::local_tempdir())
  args <- docx_publish_args(fs::path(tmp, "manuscripts", "paper.qmd"))
  expect_equal(args, c(
    paste0("--reference-doc=", fs::path(tmp, "publish", "reference.docx")),
    paste0("--lua-filter=", fs::path(tmp, "publish", "docx-format.lua"))
  ))
})

# --- render output location -------------------------------------------------

test_that("rendered_output takes the path from quarto's Output created line", {
  result <- list(stdout = "", stderr = "pandoc ...\nOutput created: _output/paper.docx\n")
  expect_equal(
    rendered_output(result, wd = "/proj/manuscripts", default = "/proj/manuscripts/paper.docx"),
    fs::path("/proj/manuscripts/_output/paper.docx")
  )
})

test_that("rendered_output falls back to the sibling path", {
  result <- list(stdout = "", stderr = "no marker here")
  expect_equal(
    rendered_output(result, wd = "/proj", default = "/proj/paper.docx"),
    "/proj/paper.docx"
  )
})

# --- upload guard -----------------------------------------------------------

test_that("upload_to_gdrive errors clearly when the DOCX is missing", {
  expect_error(
    upload_to_gdrive(tempfile(fileext = ".docx"), "paper"),
    "Rendered DOCX not found"
  )
})

# --- misc -------------------------------------------------------------------

test_that("check_package errors when package is not installed", {
  expect_error(check_package("_not_a_real_package_"), "is required but not installed")
})
